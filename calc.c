// Name Janis Vucens
// https://github.com/Janisvucensrtu/calc
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: <every AI tool, website and book you used; details in comments where you used them>

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define STUDENT_NAME "janis"
#define STUDENT_LASTNAME "vucens"
#define STUDENT_ID "251RDC065"

typedef enum { T_INT, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_EOL, T_BAD } TokKind;

typedef struct {
    TokKind kind;
    size_t pos;
    long long val;
} Token;

typedef struct {
    char *buf;
    size_t len;
    size_t pos;
    Token tok;
    size_t err;
} Parser;

typedef struct { char *data; size_t len, cap; } Buffer;

static void *xrealloc(void *p, size_t n) {
    void *q = realloc(p, n ? n : 1);
    if (!q) {
        fprintf(stderr, "calc: out of memory\n");
        exit(1);
    }
    return q;
}

static void buf_append(Buffer *b, const char *s, size_t n) {
    if (b->len + n > b->cap) {
        size_t nc = b->cap ? b->cap : 4096;
        while (nc < b->len + n) nc *= 2;
        b->data = xrealloc(b->data, nc);
        b->cap = nc;
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
}


static void advance(Parser *p) {
    char *b = p->buf;
    size_t i = p->pos;
    Token t;

    while (i < p->len && (b[i] == ' ' || b[i] == '\t')) i++;
    t.pos = i;
    t.val = 0;

    if (i >= p->len || b[i] == '\n') {
        t.kind = T_EOL;
        p->pos = i;
        p->tok = t;
        return;
    }
    if (isdigit((unsigned char)b[i])) {
        unsigned long long v = 0;
        while (isdigit((unsigned char)b[i])) {
            v = v * 10u + (unsigned long long)(b[i] - '0');
            i++;
        }
        t.kind = T_INT;
        t.val = (long long)v;
        p->pos = i;
        p->tok = t;
        return;
    }
    p->pos = i + 1;
    switch (b[i]) {
    case '+': t.kind = T_PLUS; break;
    case '-': t.kind = T_MINUS; break;
    case '/': t.kind = T_SLASH; break;
    case '(': t.kind = T_LPAREN; break;
    case ')': t.kind = T_RPAREN; break;
    case '*':
        if (b[i + 1] == '*') {t.kind = T_POW; p->pos = i + 2; }
        else t.kind = T_STAR;
        break;
    default: t.kind = T_BAD; break;
    }
    p->tok = t;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s file\n", argv[0]);
        return 2;
    }
    char *data = NULL;
    size_t len = 0;
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("open");
        return 1;
    }
    data = malloc(1 << 20);
    len = fread(data, 1, (1 << 20) - 1, f);
    fclose(f);
    data[len] = '\0';
    Parser p;
    memset(&p, 0, sizeof p);
    p.buf = data;
    p.len = len;
    for (;;) {
        advance(&p);
        printf("kind=%d pos=%zu val=%lld\n", p.tok.kind, p.tok.pos, p.tok.val);
        if (p.tok.kind == T_EOL) {
            if (p.pos >= p.len) break;
            p.pos++;
        }
    }
    free(data);
    return 0;
}