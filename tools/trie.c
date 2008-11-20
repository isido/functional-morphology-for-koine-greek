/*
  A Trie ADT for insertion and traversal.
  Copyright (C) 2004  Author: Markus Forsberg
  
  Please send bug reports to: markus@cs.chalmers.se

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/************************ Defines **************************************/
#define SIZE 256
#define MAXLINE 1000
#define DOT 1000
#define DOTLINE 50000
#define PRINTDOTS 1
#define ACCEPT_ZERO 1

/************************* Types ***************************************/
typedef struct Value {
  char *string;
  void *next;
} Value;

typedef struct {
  Value *value;
  void *list;
} Trie;

typedef struct List_el {
  char c;
  Trie *trie;
  struct List_el *next;
} List;
/***************** Functions on Values ********************************/

Value* addValue(Value *val, char *s){
  Value *n_val;
  n_val = malloc(sizeof(Value));
  n_val -> string = s;
  if(val != NULL)
    n_val -> next = val;
  else
    n_val -> next = NULL;
  return n_val;
}

void printAnalysis(Value *value, char *word){
  printf("%s:\n", word);
  if(value == NULL)
    printf("\tNONE\n");
  else{
    while(value != NULL) {
      printf("\t%s\n", value -> string);
      value = value -> next;
    }
  }
}

/*****************  List functions *************************************/

List* addList(List* list, Trie *trie, char c){
  List *new = malloc(sizeof(List));
  new -> c = c;
  new -> trie = trie;
  new -> next = list;
  return new;
}

Trie* lookup(List *list, char cl){
  List *list_traversal;
  list_traversal = list;
  while(1){
    if(list_traversal == NULL){
      return NULL;
    }
    else{
      if(list_traversal -> c == cl)
	return list_traversal -> trie;
      else
	list_traversal = list_traversal -> next;
    }
  }
}

/*****************  Trie functions *************************************/

Trie* newTrie(){
  Trie *trie;
  trie = (Trie*) malloc(sizeof(Trie));
  trie -> list = NULL;
  trie -> value = NULL;
  return trie;
}

Trie* add(Trie *trie, char c){
  Trie *t;
  List *l;
  t = lookup(trie -> list,c);
  if(t != NULL)
    return t;
  else{
    t = newTrie();
    l = addList(trie -> list,t,c);
    trie -> list = l;
    return t;
  }
}

Trie* move(Trie *trie, char c){
  return lookup(trie -> list,c);
}

void insert(Trie *trie, char *word, char *result){
  Trie *traversal_ptr;
  char *result_copy;
  int index;
  
  result_copy = malloc(sizeof(char)*(strlen(result)+1));

  traversal_ptr = trie;

  strcpy(result_copy,result);

  for(index=0 ; index < strlen(word) ; index++){
    traversal_ptr = add(traversal_ptr,word[index]);
  }
  traversal_ptr -> value = addValue(traversal_ptr -> value,result_copy);
}

Value* accept(Trie *trie,char *word){
  Trie* traversal_ptr = trie;
  int index;
  
  /* Traverse the word */
  for(index=0 ; index < strlen(word) ; index++){
    traversal_ptr = move(traversal_ptr, word[index]);
    if(traversal_ptr == NULL) 
      return NULL;
  }
  
  /* Final state reached */
  return traversal_ptr -> value;
}

/************* File operations ************************************/

/* Read a line from file */
int fgetline(FILE *fp, char line[], int max)
{
  int nch = 0;
  int c;
  max = max - 1;	
  
  while((c = getc(fp)) != EOF)
    {
      if(c == '\n')
	break;
      if(nch < max)
	{
	  line[nch] = c;
	  nch = nch + 1;
	}
    }
  if(c == EOF && nch == 0) return EOF;
  
  line[nch] = '\0';
  return nch;
}

int approved(char c){
  if(c == '.' || c == ',' || c == '!' || c == ';')
    return 0;
  return 1;
}

int fgetword(FILE *fp, char line[], int max)
{
  int nch = 0;
  int c;
  max = max - 1;	
  
  while((c = getc(fp)) != EOF)
    {
      if(c == '\n' || c == '\t' || c == ' ')
	break;
      if(nch < max)
	{
	  if(approved(c)){
	    line[nch] = c;
	    nch = nch + 1;
	  }
	}
    }
  if(c == EOF && nch == 0) return EOF;
  
  line[nch] = '\0';
  return nch;
}

/* Splits a line into two, with the delimiter ':' */
void split(char *line, char *fst, char *snd){
  int index,diff,size;

  size = strlen(line);

  for(index = 0; index < size; index++){
    if(line[index] == ':'){
      fst[index] = '\0';
      break;
    }
    else{
      fst[index] = line[index];
    }
  }

  index++;
  diff = index;
  
  for(; index < size; index++){
    snd[index - diff] = line[index];
  }
  snd[index-diff] = '\0';
}

Trie* build(char *filename, int *result){
  Trie *trie = newTrie();
  FILE *file_ptr;
  char line[MAXLINE];
  char fst[MAXLINE];
  char snd[MAXLINE];
  int status;
  int count;

  count = 1;
  file_ptr = fopen(filename,"r");
  
  while(1){
    
    status = fgetline(file_ptr, line, MAXLINE);
    if(status == EOF)
      break;
    split(line,fst,snd);
    if(strlen(snd) > 0 || ACCEPT_ZERO){
      if(PRINTDOTS){
	if(count%DOT == 0){
	  fprintf(stderr,".");
	  if(count%DOTLINE == 0)
	    fprintf(stderr,"\n");
	}
      }
      insert(trie,fst,snd);
      count++;
    }
  }
  (*result) = count;
  return trie;
}

void run(Trie *trie){
  char line[MAXLINE];
  Value *val;

  while(fgetword(stdin, line, MAXLINE) != EOF){
    if(strlen(line) > 0){
      val = accept(trie,line);
      printAnalysis(val,line);
    }
  }
}

/************* Main function **********************/

int main(int argc, char *argv[]){
  Trie *trie;
  char *filename;
  clock_t start,end;
  double cpu_time_used;
  int count;
  if(argc < 2){
    fprintf(stderr,"usage: trie <lexicon>\n");
    exit(EXIT_FAILURE);
  }
  filename = argv[1];
  fprintf(stderr, "\n-----------------------------------------------------\n");
  fprintf(stderr, "|              C-Trie 0.1, April 2004               |\n");
  fprintf(stderr, "|---------------------------------------------------|\n");
  fprintf(stderr, "|             Author: Markus Forsberg               |\n");
  fprintf(stderr, "|                                                   |\n");
  fprintf(stderr, "|   Send bug reports to: markus@cs.chalmers.se      |\n");
  fprintf(stderr, "-----------------------------------------------------\n\n");
  fprintf(stderr, "[Reading lexicon read from '%s'...]\n\n",filename);
  
  /* If we print dots, give an explanation */
  // if(PRINTDOTS)
  //  fprintf(stderr,"['.' = %d words]\n",DOT);
  
  /** Building the Trie */
  start = clock();
  trie = build(filename,&count);
  end = clock();
  count = count/1000;
  cpu_time_used = ((double)(end - start))/CLOCKS_PER_SEC;
  fprintf(stderr,"\n\n[ Number of entries :%7dk entries ]\n", count);
  fprintf(stderr,"[ Build CPU time    :%8.2f seconds ]\n\n", cpu_time_used);

  /* Running */
  start = clock();  
  run(trie);
  end = clock();
  cpu_time_used = ((double)(end - start))/CLOCKS_PER_SEC;
  fprintf(stderr,"\n\n[ Analysis CPU time :%8.2f seconds ]\n\n", cpu_time_used);
  return EXIT_SUCCESS;
}
