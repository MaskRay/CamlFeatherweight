#pragma once

#define INVALID_EXN 3
void not_implemented(const char *msg);
void invalid_argument(const char *msg);
void fatal_error(const char *msg);
void fatal_error_fmt(const char *fmt, ...);
