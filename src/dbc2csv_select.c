/*
 * dbc2csv_select.c
 *
 * Selective DBC -> CSV conversion.
 *
 * The DBC data is decompressed by blast() and only the requested
 * DBF fields are written to the output CSV.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <R.h>

#include "blast.h"

#define CHUNK 4096
#define MAX_ERR 255

#define HEADER_OFFSET 8
#define CRC_OFFSET 4

#define DBF_FIELD_DESCRIPTOR_SIZE 32
#define DBF_FIELD_NAME_SIZE 11

/* ---------------------------------------------------------------
 * Input context
 * --------------------------------------------------------------- */

struct input_context {
    FILE *fp;
    unsigned char buffer[CHUNK];
};

/* ---------------------------------------------------------------
 * blast input callback
 * --------------------------------------------------------------- */

static unsigned inf(void *how, unsigned char **buf)
{
    struct input_context *ctx =
        (struct input_context *)how;

    *buf = ctx->buffer;

    return (unsigned)fread(
        ctx->buffer,
        1,
        CHUNK,
        ctx->fp
    );
}

/* ---------------------------------------------------------------
 * Error helper
 * --------------------------------------------------------------- */

static void set_error(
    char **error_str,
    const char *message
)
{
    if (error_str == NULL || error_str[0] == NULL)
        return;

    strncpy(
        error_str[0],
        message,
        MAX_ERR
    );

    error_str[0][MAX_ERR] = '\0';
}

/* ---------------------------------------------------------------
 * DBF field information
 * --------------------------------------------------------------- */

typedef struct {

    char name[DBF_FIELD_NAME_SIZE + 1];

    char type;

    unsigned int offset;

    unsigned int length;

    unsigned int decimals;

} dbf_field;

/* ---------------------------------------------------------------
 * Selected field
 * --------------------------------------------------------------- */

typedef struct {

    int source_index;

    dbf_field field;

} selected_field;

/* ---------------------------------------------------------------
 * Selective output context
 * --------------------------------------------------------------- */

typedef struct {

    FILE *output;

    unsigned char *record_buffer;

    size_t record_size;

    size_t record_used;

    uint32_t number_of_records;

    unsigned int nfields;

    selected_field *fields;

    int header_written;

} select_context;

/* ---------------------------------------------------------------
 * Trim whitespace from a DBF field name
 * --------------------------------------------------------------- */

static void trim_field_name(char *name)
{
    int len;

    len = (int)strlen(name);

    while (
        len > 0 &&
        isspace((unsigned char)name[len - 1])
    ) {
        name[len - 1] = '\0';
        len--;
    }
}

/* ---------------------------------------------------------------
 * Case-insensitive field comparison
 * --------------------------------------------------------------- */

static int field_name_equal(
    const char *a,
    const char *b
)
{
#ifdef _WIN32
    return _stricmp(a, b) == 0;
#else
    return strcasecmp(a, b) == 0;
#endif
}

/* ---------------------------------------------------------------
 * Read little-endian 16-bit integer
 * --------------------------------------------------------------- */

static uint16_t read_uint16_le(
    const unsigned char *p
)
{
    return (uint16_t)(
        ((uint16_t)p[0]) |
        ((uint16_t)p[1] << 8)
    );
}

/* ---------------------------------------------------------------
 * Read little-endian 32-bit integer
 * --------------------------------------------------------------- */

static uint32_t read_uint32_le(
    const unsigned char *p
)
{
    return (uint32_t)(
        ((uint32_t)p[0]) |
        ((uint32_t)p[1] << 8) |
        ((uint32_t)p[2] << 16) |
        ((uint32_t)p[3] << 24)
    );
}

/* ---------------------------------------------------------------
 * Write CSV field safely
 * --------------------------------------------------------------- */

static int write_csv_value(
    FILE *output,
    const unsigned char *data,
    unsigned int length
)
{
    unsigned int i;

    int needs_quotes = 0;

    for (i = 0; i < length; i++) {

        if (
            data[i] == ',' ||
            data[i] == '"' ||
            data[i] == '\r' ||
            data[i] == '\n'
        ) {
            needs_quotes = 1;
            break;
        }
    }

    if (needs_quotes)
        fputc('"', output);

    for (i = 0; i < length; i++) {

        unsigned char c = data[i];

        if (c == '"') {

            fputc('"', output);
            fputc('"', output);

        } else {

            fputc(c, output);
        }
    }

    if (needs_quotes)
        fputc('"', output);

    return ferror(output) ? 0 : 1;
}

/* ---------------------------------------------------------------
 * Write CSV header
 * --------------------------------------------------------------- */

static int write_csv_header(
    select_context *ctx
)
{
    unsigned int i;

    for (i = 0; i < ctx->nfields; i++) {

        if (i > 0)
            fputc(',', ctx->output);

        fputs(
            ctx->fields[i].field.name,
            ctx->output
        );
    }

    fputc('\n', ctx->output);

    ctx->header_written = 1;

    return ferror(ctx->output) ? 0 : 1;
}

/* ---------------------------------------------------------------
 * Process one complete DBF record
 * --------------------------------------------------------------- */

static int process_record(
    select_context *ctx
)
{
    unsigned int i;

    /*
     * DBF records begin with one byte containing the deletion flag.
     *
     * Therefore the first field begins at byte offset 1.
     */

    for (i = 0; i < ctx->nfields; i++) {

        selected_field *selected =
            &ctx->fields[i];

        unsigned int offset =
            selected->field.offset;

        unsigned int length =
            selected->field.length;

        if (i > 0)
            fputc(',', ctx->output);

        if (
            offset + length >
            ctx->record_size
        ) {
            return 0;
        }

        if (!write_csv_value(
                ctx->output,
                ctx->record_buffer + offset,
                length
            )) {

            return 0;
        }
    }

    fputc('\n', ctx->output);

    return ferror(ctx->output) ? 0 : 1;
}

/* ---------------------------------------------------------------
 * blast output callback
 *
 * IMPORTANT:
 * blast() can split DBF records between output blocks.
 * Therefore the bytes are accumulated until a complete DBF
 * record is available.
 * --------------------------------------------------------------- */

static int outf_select(
    void *how,
    unsigned char *buf,
    unsigned len
)
{
    select_context *ctx =
        (select_context *)how;

    unsigned char *ptr = buf;

    unsigned remaining = len;

    while (remaining > 0) {

        size_t needed =
            ctx->record_size -
            ctx->record_used;

        size_t copy_size =
            remaining < needed
                ? remaining
                : needed;

        memcpy(
            ctx->record_buffer +
                ctx->record_used,

            ptr,

            copy_size
        );

        ctx->record_used += copy_size;

        ptr += copy_size;

        remaining -= (unsigned)copy_size;

        /*
         * A complete DBF record has been accumulated.
         */

        if (
            ctx->record_used ==
            ctx->record_size
        ) {

            if (!process_record(ctx))
                return 1;

            ctx->record_used = 0;
        }
    }

    return 0;
}

/* ---------------------------------------------------------------
 * Read DBF header and identify selected fields
 * --------------------------------------------------------------- */

static int parse_dbf_header(
    FILE *input,
    unsigned int header_size,
    unsigned int record_size,
    uint32_t number_of_records,
    char **fields,
    unsigned int nfields,
    selected_field *selected,
    char **error_str
)
{
    unsigned char descriptor[
        DBF_FIELD_DESCRIPTOR_SIZE
    ];

    unsigned int field_count;

    unsigned int i;

    unsigned int current_offset = 1;

    long field_start;

    /*
     * DBF field descriptors start after:
     *
     *   32-byte DBF header
     *
     * and continue until 0x0D.
     */

    field_start = 32;

    field_count =
        (header_size - 33) /
        DBF_FIELD_DESCRIPTOR_SIZE;

    for (i = 0; i < field_count; i++) {

        char field_name[
            DBF_FIELD_NAME_SIZE + 1
        ];

        unsigned int j;

        int requested_index = -1;

        if (
            fseek(
                input,
                field_start +
                    (long)i *
                    DBF_FIELD_DESCRIPTOR_SIZE,
                SEEK_SET
            ) != 0
        ) {

            set_error(
                error_str,
                "Unable to seek to DBF field descriptor"
            );

            return 0;
        }

        if (
            fread(
                descriptor,
                1,
                DBF_FIELD_DESCRIPTOR_SIZE,
                input
            ) != DBF_FIELD_DESCRIPTOR_SIZE
        ) {

            set_error(
                error_str,
                "Unable to read DBF field descriptor"
            );

            return 0;
        }

        memcpy(
            field_name,
            descriptor,
            DBF_FIELD_NAME_SIZE
        );

        field_name[
            DBF_FIELD_NAME_SIZE
        ] = '\0';

        trim_field_name(field_name);

        /*
         * Check whether this field was requested.
         */

        for (j = 0; j < nfields; j++) {

            if (
                field_name_equal(
                    field_name,
                    fields[j]
                )
            ) {

                requested_index = (int)j;

                break;
            }
        }

        if (requested_index >= 0) {

            selected[requested_index]
                .source_index = (int)i;

            strncpy(
                selected[requested_index]
                    .field.name,
                field_name,
                DBF_FIELD_NAME_SIZE
            );

            selected[requested_index]
                .field.name[
                    DBF_FIELD_NAME_SIZE
                ] = '\0';

            selected[requested_index]
                .field.type =
                    descriptor[11];

            selected[requested_index]
                .field.offset =
                    current_offset;

            selected[requested_index]
                .field.length =
                    descriptor[16];

            selected[requested_index]
                .field.decimals =
                    descriptor[17];
        }

        current_offset += descriptor[16];
    }

    /*
     * Verify that every requested field was found.
     */

    for (i = 0; i < nfields; i++) {

        if (
            selected[i].field.length == 0
        ) {

            char msg[MAX_ERR + 1];

            snprintf(
                msg,
                sizeof(msg),
                "Column '%s' not found in DBF",
                fields[i]
            );

            set_error(
                error_str,
                msg
            );

            return 0;
        }
    }

    /*
     * Verify record size.
     */

    if (current_offset != record_size) {

        set_error(
            error_str,
            "DBF record size does not match field definitions"
        );

        return 0;
    }

    return 1;
}

/* ---------------------------------------------------------------
 * Main function exposed to R
 * --------------------------------------------------------------- */

void dbc2csv_select(
    char **input_file,
    char **output_file,
    char **fields,
    int *nfields,
    int *ret_code,
    char **error_str
)
{
    FILE *input = NULL;

    FILE *output = NULL;

    unsigned char raw_header[2];

    uint16_t dbc_header_size;

    unsigned char *dbf_header = NULL;

    uint32_t number_of_records;

    uint16_t dbf_header_size;

    uint16_t record_size;

    selected_field *selected = NULL;

    select_context ctx;

    struct input_context input_ctx;

    int ret;

    unsigned int i;

    *ret_code = 0;

    memset(
        &ctx,
        0,
        sizeof(ctx)
    );

    /*
     * Validate arguments.
     */

    if (
        input_file == NULL ||
        input_file[0] == NULL
    ) {

        *ret_code = -1;

        set_error(
            error_str,
            "Invalid input file"
        );

        return;
    }

    if (
        output_file == NULL ||
        output_file[0] == NULL
    ) {

        *ret_code = -2;

        set_error(
            error_str,
            "Invalid output file"
        );

        return;
    }

    if (
        fields == NULL ||
        nfields == NULL ||
        *nfields <= 0
    ) {

        *ret_code = -3;

        set_error(
            error_str,
            "At least one field must be selected"
        );

        return;
    }

    /*
     * Open DBC.
     */

    input = fopen(
        input_file[0],
        "rb"
    );

    if (input == NULL) {

        *ret_code = -4;

        {
            char msg[MAX_ERR + 1];

            snprintf(
                msg,
                sizeof(msg),
                "Error opening input file: %s",
                strerror(errno)
            );

            set_error(
                error_str,
                msg
            );
        }

        return;
    }

    /*
     * Read DBC header size.
     */

    if (
        fseek(
            input,
            HEADER_OFFSET,
            SEEK_SET
        ) != 0
    ) {

        *ret_code = -5;

        set_error(
            error_str,
            "Unable to seek to DBC header"
        );

        fclose(input);

        return;
    }

    if (
        fread(
            raw_header,
            1,
            2,
            input
        ) != 2
    ) {

        *ret_code = -6;

        set_error(
            error_str,
            "Unable to read DBC header size"
        );

        fclose(input);

        return;
    }

    dbc_header_size =
        (uint16_t)(
            raw_header[0] |
            ((uint16_t)raw_header[1] << 8)
        );

    /*
     * Read DBF header.
     */

    dbf_header =
        (unsigned char *)malloc(
            dbc_header_size
        );

    if (dbf_header == NULL) {

        *ret_code = -7;

        set_error(
            error_str,
            "Memory allocation failed for DBF header"
        );

        fclose(input);

        return;
    }

    rewind(input);

    if (
        fread(
            dbf_header,
            1,
            dbc_header_size,
            input
        ) != dbc_header_size
    ) {

        *ret_code = -8;

        set_error(
            error_str,
            "Unable to read complete DBF header"
        );

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Read DBF metadata.
     */

    if (dbc_header_size < 32) {

        *ret_code = -9;

        set_error(
            error_str,
            "Invalid DBF header"
        );

        free(dbf_header);

        fclose(input);

        return;
    }

    number_of_records =
        read_uint32_le(
            dbf_header + 4
        );

    dbf_header_size =
        read_uint16_le(
            dbf_header + 8
        );

    record_size =
        read_uint16_le(
            dbf_header + 10
        );

    if (record_size == 0) {

        *ret_code = -10;

        set_error(
            error_str,
            "Invalid DBF record size"
        );

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Allocate selected-field information.
     */

    selected =
        (selected_field *)calloc(
            (size_t)*nfields,
            sizeof(selected_field)
        );

    if (selected == NULL) {

        *ret_code = -11;

        set_error(
            error_str,
            "Memory allocation failed for selected fields"
        );

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Parse DBF field descriptors.
     */

    if (!parse_dbf_header(
            input,
            dbf_header_size,
            record_size,
            number_of_records,
            fields,
            (unsigned int)*nfields,
            selected,
            error_str
        )
    ) {

        *ret_code = -12;

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Open CSV output.
     */

    output = fopen(
        output_file[0],
        "wb"
    );

    if (output == NULL) {

        *ret_code = -13;

        {
            char msg[MAX_ERR + 1];

            snprintf(
                msg,
                sizeof(msg),
                "Error creating output file: %s",
                strerror(errno)
            );

            set_error(
                error_str,
                msg
            );
        }

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Initialize streaming context.
     */

    ctx.output = output;

    ctx.record_size = record_size;

    ctx.record_used = 0;

    ctx.number_of_records =
        number_of_records;

    ctx.nfields =
        (unsigned int)*nfields;

    ctx.fields = selected;

    ctx.record_buffer =
        (unsigned char *)malloc(
            record_size
        );

    if (ctx.record_buffer == NULL) {

        *ret_code = -14;

        set_error(
            error_str,
            "Memory allocation failed for DBF record buffer"
        );

        fclose(output);

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Write selected-column header.
     */

    if (!write_csv_header(&ctx)) {

        *ret_code = -15;

        set_error(
            error_str,
            "Error writing CSV header"
        );

        free(ctx.record_buffer);

        fclose(output);

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Jump to compressed data.
     *
     * DBC structure:
     *
     *   DBF header
     *   CRC32
     *   compressed data
     */

    if (
        fseek(
            input,
            (long)dbc_header_size +
                CRC_OFFSET,
            SEEK_SET
        ) != 0
    ) {

        *ret_code = -16;

        set_error(
            error_str,
            "Unable to seek to compressed DBC data"
        );

        free(ctx.record_buffer);

        fclose(output);

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Decompress using the existing blast implementation.
     */

    input_ctx.fp = input;

    ret = blast(
        inf,
        &input_ctx,
        outf_select,
        &ctx
    );

    if (ret != 0) {

        *ret_code = ret;

        {
            char msg[MAX_ERR + 1];

            snprintf(
                msg,
                sizeof(msg),
                "DBC decompression failed (code %d)",
                ret
            );

            set_error(
                error_str,
                msg
            );
        }

        free(ctx.record_buffer);

        fclose(output);

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * A valid DBF stream should end exactly at a record boundary.
     */

    if (ctx.record_used != 0) {

        *ret_code = -17;

        set_error(
            error_str,
            "Incomplete DBF record at end of decompressed data"
        );

        free(ctx.record_buffer);

        fclose(output);

        free(selected);

        free(dbf_header);

        fclose(input);

        return;
    }

    /*
     * Cleanup.
     */

    free(ctx.record_buffer);

    fclose(output);

    free(selected);

    free(dbf_header);

    fclose(input);

    *ret_code = 0;
}
