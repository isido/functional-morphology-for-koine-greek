#ifndef _TRIELIB
#define _TRIELIB


#include<stdbool.h>

/************************* Types ***************************************/
typedef struct {
  const char* string;
  void *next;
} Value_Struct;

typedef Value_Struct* Values ;

typedef struct {
  Values value;
  void*  list;
} Trie_Struct;

typedef Trie_Struct* Trie;

typedef struct List_el {
  char c;
  Trie trie;
  struct List_el *next;
} List_Struct;

typedef List_Struct* List ; 


void build(const char *filename);

char *strreverse(char *str);

void reversed();

void no_count();

void lookup_t(const char* word);

char* next();

bool in_t(const char* word);

void insert_t(const char* word, const char* result);

void empty();

bool more();

int getNumber(const char* word);

void start();

void stop();

void free_trie();

#endif
