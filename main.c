#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef enum
{
	a_number,
	a_string,
	a_null,
	a_var,
	//a_sexp,
} atom_kind;

typedef struct
{
	atom_kind kind;
	union {
		double number;
		char* string;
	};
} atom;

atom _null = (atom){a_null, {0}};

typedef struct
{
	atom* contents;
	size_t count;
	size_t size;
} list;

char** var_names = 0;
atom* vars = 0;
size_t vars_count;
size_t vars_size;

void* alloc(size_t sz)
{
	return malloc(sz);
}

void* re_alloc(void* buf, size_t sz)
{
	return realloc(buf, sz);
}

int is_valid(char c)
{
	if (c == ')') return 0;
	if (isspace(c)) return 0;
	if (!isprint(c)) return 0;
	return 1;
}

list make_list()
{
	list ret = {0};
	ret.size = 3;
	ret.count = 0;
	ret.contents = alloc(sizeof(atom) * ret.size);
	return ret;
}

void add_lists_raw(
	void*** contents,
	size_t contents_count,
	size_t* count,
	size_t* size,
	void* item,
	size_t sz
) {
	while (*count >= *size)
	{
		*size *= 2;
		*contents = re_alloc(*contents, *size);
	}
	for (size_t i = 0; i < contents_count; i++)
	{
		void** _contents = contents[contents_count];
		memcpy(*_contents, item, sz);
	}
	(*count)++;
}

void add_list(list* l, atom a)
{
	void** _contents = (void**)&l->contents;
	add_lists_raw(
		(void***)&_contents,
		1,
		&l->count,
		&l->size,
		&a,
		sizeof(a)
	);
}

void define_var(const char* name, atom value)
{
	void** fuck[] = {
		(void**)&var_names,
		(void**)&value
	};
	add_lists_raw(
		fuck,
		2,
		&vars_count,
		&vars_size,
		&name,
		sizeof(char*)
	);
}

atom eval_sexp(const char** sexp_p);

atom parse_atom(const char** sexp_p)
{
	const char* sexp = *sexp_p;
	while (isspace(*sexp)) sexp++;
	if (isdigit(*sexp))
	{
		int value = atoi(sexp);
		atom a = {.kind = a_number, .number = value};
		while (isdigit(*++sexp));
		*sexp_p = sexp;
		return a;
	}
	if (*sexp == '"')
	{
		sexp++;
		char* end = strchr(sexp, '"');
		if (!end)
		{
			*sexp_p = sexp;
			return _null;
		}
		size_t sz = end - sexp;
		char* str = alloc(sz + 1);
		memcpy(str, sexp, sz);
		str[sz] = 0;

		atom a = {.kind = a_string, .string = str};
		*sexp_p = end + 1;
		return a;
	}
	if (!isprint(*sexp))
	{
		*sexp_p = sexp;
		return _null;
	}

	size_t sz = 0;
	while (is_valid(sexp[sz])) sz++;

	char* str = alloc(sz + 1);
	memcpy(str, sexp, sz);
	str[sz] = 0;

	atom a = {.kind = a_var, .string = str};
	*sexp_p = sexp + sz;

	return a;
}

atom get_var(atom a)
{
	char* name = a.string;
	for (size_t i = 0; i < vars_count; i++)
	{
		if (!strcmp(name, var_names[i])) return vars[i];
	}
	return _null;
}

void print(atom a)
{
	switch (a.kind)
	{
	case a_number: printf("%lf", a.number); break;
	case a_string: printf("%s", a.string); break;
	case a_var: print(get_var(a));
	default: printf("ERR"); break;
	}
}

int add_arg(list* l, const char** sexp_p)
{
	while (isspace(**sexp_p)) (*sexp_p)++;
	if (**sexp_p == ')') return 0;
	if (**sexp_p == '(')
	{
		atom a = eval_sexp(sexp_p);
		if (a.kind == a_null) return 0;
		add_list(l, a);
		return 1;
	}

	atom a = parse_atom(sexp_p);
	if (a.kind == a_null) return 0;
	add_list(l, a);
	return 1;
}

double to_number(atom a)
{
	switch (a.kind)
	{
	case a_number: return a.number;
	case a_var: return to_number(get_var(a));
	default: return 0;
	}
}

atom eval_fn(const char* name, list arglist)
{
	if (!strcmp(name, "+"))
	{
		atom a = {.kind = a_number, .number = 0};
		for (size_t i = 0; i < arglist.count; i++)
			a.number += to_number(arglist.contents[i]);
		return a;
	}
	if (!strcmp(name, "print"))
	{
		for (size_t i = 0; i < arglist.count; i++)
		{
			if (i) printf(" ");
			print(arglist.contents[i]);
		}
		printf("\n");
		return _null;
	}
	if (!strcmp(name, "defvar"))
	{
		if (arglist.count != 2)
		{
			fprintf(stderr, "defvar: not 2 args\n");
			return _null;
		}
		if (arglist.contents[0].kind != a_var)
		{
			fprintf(stderr, "defvar: arg1 is not var\n");
			return _null;
		}
		define_var(arglist.contents[0].string, arglist.contents[1]);
	}

	fprintf(stderr, "unknown function '%s'\n", name);
	return _null;
}

atom eval_sexp(const char** sexp_p)
{
	const char* sexp = *sexp_p;
	while (isspace(*sexp)) sexp++;
	if (*sexp++ != '(')
	{
		*sexp_p = sexp;
		return _null;
	}

	const char* cp = sexp;
	int fn_len = 0;
	while (is_valid(*cp++)) fn_len++;

	char* fn_name = alloca(fn_len + 1);
	for (int i = 0; i < fn_len; i++) fn_name[i] = sexp[i];
	fn_name[fn_len] = 0;
	sexp += fn_len;

	list arglist = make_list();

	while (*sexp != ')') if (!add_arg(&arglist, &sexp))
	{
		printf("returning _null\n");
		*sexp_p = sexp;
		return _null;
	}
	while (isspace(*sexp)) sexp++;

	if (*sexp == ')') sexp++;
	*sexp_p = sexp;

	return eval_fn(fn_name, arglist);
}

int main(int argc, const char** argv)
{
	if (argc < 2) return printf("no file provided\n");

	FILE* fp = fopen(argv[1], "r");
	if (!fp) return printf("could not open '%s'\n");

	fseek(fp, 0, SEEK_END);
	size_t sz = ftell(fp);
	fseek(fp, 0, SEEK_SET);

	char* buffer = malloc(sz + 1);
	buffer[sz] = 0;

	fread(buffer, sz, 1, fp);

	printf("evaluating buffer:\n");
	printf("%s ====== output:\n", buffer);

	vars_size = 3;
	vars_count = 0;
	vars = alloc(sizeof(atom) * vars_size);
	var_names = alloc(sizeof(char*) * vars_size);

	const char* cursor = buffer;
	while (*cursor)
	{
		eval_sexp(&cursor);
		while (isspace(*cursor)) cursor++;
	}
}

