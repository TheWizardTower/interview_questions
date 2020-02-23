#include <argp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void show_usage() { printf("TODO"); }

struct command_line_options {
  int word_count;
  char *filename;
};

struct word_list {
  int length;
  char **word_list;
};

struct word_count {
  int count;
  char *word;
};

struct concordance {
  int length;
  struct word_count *wc;
};

const char *argp_program_version = "concordance_c 0.0.1";
const char *argp_program_bug_address = "merlinfmct87@gmail.com";
static char doc[] =
    "This reads input from stdin or a file and builds a concordance. From "
    "this, it prints the n most common words in the input.";
static char args_doc[] = "concord_c";
static struct argp_option options[] = {
    {"number", 'n', "num", 0, "Number of words to show.", 0},
    {"filename", 'f', "file", 0,
     "File to read from. If not provided, read from stdin.", 0},
};

static error_t parse_opt(int key, char *arg, struct argp_state *state) {
  struct command_line_options *cl_opt = state->input;
  switch (key) {
  case 'f':
    cl_opt->filename = arg;
    break;
  case 'n':
    cl_opt->word_count = atoi(arg);
    break;
  case ARGP_KEY_END:
    break;
  default:
    return ARGP_ERR_UNKNOWN;
  }

  return 0;
}

static struct argp argp = {options, parse_opt, args_doc, doc, 0, 0, 0};

char *read_entire_file(char *filename) {
  printf("Reading from file %s\n", filename);
  struct stat stats;
  if (stat(filename, &stats)) {
    printf("Could not stat file.\n");
    exit(-1);
  }
  char *buffer = calloc(stats.st_size, sizeof(char));
  FILE *fp = fopen(filename, "r");
  if (!fp) {
    printf("Could not open file to read.");
    exit(-1);
  }

  /* printf("Malloc point one.\n"); */
  int read_size = fread(buffer, sizeof(char), stats.st_size, fp);
  if (read_size == 0) {
    printf("Could not read from file.\n");
    exit(-1);
  }
  fclose(fp);

  return buffer;
}

#define STEP_SIZE 10240
char *read_from_stdin() {
  char *storage = calloc(STEP_SIZE, sizeof(char));
  char *buffer = calloc(STEP_SIZE, sizeof(char));
  int byteCount = 0;
  int storage_size = STEP_SIZE;
  while (fgets(buffer, STEP_SIZE, stdin) != NULL) {
    // read up to BUFFER_SIZE into buffer
    // store # of bytes read as temp1
    int temp1 = strnlen(buffer, INT64_MAX);
    byteCount += temp1;
    if (byteCount > storage_size) {
      int old_size = storage_size;
      storage_size *= 2;
      storage = realloc(storage, storage_size);
      if (!storage) {
        printf("Out of memory.\n");
        exit(-1);
      }
      // wipe end of storage with nulls
      for (int i = old_size; i < storage_size; i++) {
        storage[i] = '\0';
      }
    }
    // copy buffer into end of storage
    strncat(storage, buffer, INT64_MAX);
    // wipe buffer
    memset(buffer, 0, 1024);
  }
  return storage;
}

char *get_input(char *filename) {
  return strncmp(filename, "", INT64_MAX) ? read_entire_file(filename)
                                          : read_from_stdin();
}

void replace_whitespace(char *input) {
  int length = strnlen(input, INT64_MAX);
  for (int i = 0; i < length; i++) {
    if (isspace(input[i])) {
      input[i] = '\n';
    }
  }
}

struct word_list split(char *input) {
  char *token = NULL;
  int count = 0;
  int length = strnlen(input, INT64_MAX);
  for (int i = 0; i < length; i++) {
    if (input[i] == '\n') {
      count++;
    }
  }

  /* printf("Malloc point two.\n"); */
  char **word_list = calloc(count, sizeof(struct word_list));
  /* printf("Malloc point two done.\n"); */
  count = 0;
  while ((token = strsep(&input, "\n"))) {
    if (strnlen(token, INT64_MAX) == 0) {
      continue;
    }
    // add it to a hash map or a list?
    word_list[count] = calloc(strnlen(token, INT64_MAX) + 1, 1);
    strncpy(word_list[count], token, strnlen(token, INT64_MAX));
    count++;
  }
  struct word_list wl;
  wl.length = count;
  wl.word_list = word_list;

  return wl;
}

int cmpfnc(const void *a, const void *b) {
  const char **ia = (const char **)a;
  const char **ib = (const char **)b;

  int a_length = strnlen(*ia, INT64_MAX);
  int b_length = strnlen(*ib, INT64_MAX);
  return strncmp(*ia, *ib, (a_length <= b_length) ? b_length : a_length);
}

int wc_cmp_fnc(const void *a, const void *b) {
  const struct word_count *wc_a = (struct word_count *)a;
  const struct word_count *wc_b = (struct word_count *)b;

  return wc_a->count - wc_b->count;
}

struct concordance count_duplicates(struct word_list wl) {
  struct word_count *wc;
  /* printf("sizeof word_count: %lu\nlength: %d\n", (unsigned long)sizeof(struct
   * word_count), wl.length); */
  /* printf("Malloc point three.\n"); */
  wc = (struct word_count *)calloc(wl.length, sizeof(struct word_count));
  /* printf("Past malloc() call.\n"); */
  struct word_count wc_tmp;
  wc_tmp.word = "";
  int num_words = 0;
  for (int i = 0; i < wl.length; i++) {
    /* printf("Parsing %s\n", wl.word_list[i]); */
    if (strncmp(wc_tmp.word, "", INT64_MAX) == 0) {
      int string_length = strnlen(wl.word_list[i], INT64_MAX);
      wc_tmp.word = calloc(string_length + 1, 1);
      strncpy(wc_tmp.word, wl.word_list[i], string_length);
      /* printf("Strncpy result1: %s\n", wc_tmp.word); */
      wc_tmp.count = 1;
      continue;
    }
    if (strncmp(wl.word_list[i], wc_tmp.word, INT64_MAX) == 0) {
      /* printf("Duplicate for word %s found.\n", wc_tmp.word); */
      wc_tmp.count++;
      continue;
    }
    wc[num_words] = wc_tmp;
    num_words++;
    struct word_count tmp;
    int string_length = strnlen(wl.word_list[i], INT64_MAX);
    tmp.word = calloc(string_length + 1, 1);
    strncpy(tmp.word, wl.word_list[i], string_length);
    tmp.count = 1;
    wc_tmp = tmp;
    /* printf("Strncpy result2: %s\n", wc_tmp.word); */
  }

  /* printf("Adding trailing word %s.\n", wc_tmp.word); */
  if (strncmp(wc_tmp.word, "", 1) != 0) {
    wc[num_words] = wc_tmp;
    num_words++;
    /* printf("Final result: %s.\n", wc[num_words].word); */
  }

  /* for (int i = 0; i < num_words; i++) { */
  /*   printf("Word %d: %s\n", i, wc[i].word); */
  /* } */

  /* printf("Building concordance.\n"); */
  struct concordance concord;
  concord.length = num_words;
  concord.wc = wc;

  return concord;
}

void build_wordlist(struct concordance *concord, int number_argument) {
  if (concord->length <= number_argument) {
    return;
  }

  struct word_count *wc = malloc(sizeof(struct word_count) * number_argument);
  for (int i = 0; i < number_argument; i++) {
    int wc_index = i + concord->length - number_argument;
    struct word_count wc_tmp;
    int string_length = strnlen(concord->wc[wc_index].word, INT64_MAX);
    wc_tmp.word = calloc(string_length + 1, sizeof(char *));
    strncpy(wc_tmp.word, concord->wc[wc_index].word, string_length + 1);
    wc_tmp.count = concord->wc[wc_index].count;
    wc[i] = wc_tmp;
  }

  for (int i = 0; i < concord->length; i++) {
    free(concord->wc[i].word);
  }

  free(concord->wc);
  concord->length = number_argument;
  concord->wc = wc;
  return;
}

void print_list(struct concordance concord) {
  for (int i = 0; i < concord.length; i++) {
    printf("%7d %s\n", concord.wc[i].count, concord.wc[i].word);
  }
}

int main(int argc, char **argv) {
  /* printf("Hello, world\n"); */

  struct command_line_options cl_options;
  cl_options.word_count = 10;
  cl_options.filename = "";
  argp_parse(&argp, argc, argv, 0, 0, &cl_options);

  char *contents = get_input(cl_options.filename);

  replace_whitespace(contents);

  /* printf("Input post-search-and-replace:\n\n%s\n", contents); */

  struct word_list wl = split(contents);

  // We've re-allocated the relevant strings into the word_list struct, so free
  // it here.
  free(contents);

  /* printf("Word list, length %d:\n", wl.length); */
  /* for (int i = 0; i < wl.length; i++) { */
  /*   printf("word %d: %s\n", i, wl.word_list[i]); */
  /* } */

  /* printf("Sorting function starting.\n"); */

  qsort(wl.word_list, wl.length, sizeof(char *), cmpfnc);
  /* printf("Sorting function done.\n"); */

  /* for (int i = 0; i < wl.length; i++) { */
  /*   printf("word %d: %s\n", i, wl.word_list[i]); */
  /* } */
  /* printf("Sorted word list, length %d:\n", wl.length); */

  /* printf("Calling count_duplicates function.\n"); */
  struct concordance concord = count_duplicates(wl);

  free(wl.word_list);

  /* printf("Concordance, length %d:\n", concord.length); */
  /* for (int i = 0; i < concord.length; i++) { */
  /*   printf("Word %d: %s\n", i, concord.wc[i].word); */
  /* } */

  qsort(concord.wc, concord.length, sizeof(struct word_count), wc_cmp_fnc);

  /* printf("Concordance, length %d:\n", concord.length); */
  /* for (int i = 0; i < concord.length; i++) { */
  /*   printf("Word %d: %s\n", i, concord.wc[i].word); */
  /* } */

  build_wordlist(&concord, cl_options.word_count);

  /* printf("Shrunk Concordance, length %d:\n", shrunk_concord.length); */
  /* for (int i = 0; i < shrunk_concord.length; i++) { */
  /*   printf("Word %d: %s\n", i, shrunk_concord.wc[i].word); */
  /* } */

  print_list(concord);

  for (int i = 0; i < concord.length; i++) {
    free(concord.wc[i].word);
  }

  free(concord.wc);

  return 0;
}
