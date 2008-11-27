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
  GNU General Public License for more deails.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include "trie_lib.h"
#include <string.h>

/************************ Defines **************************************/
#define MAXLINE 1000
#define ACCEPT_ZERO 1
#define DELIMITER ':'
#define DOT 20000
/*******************************************************/

Trie   global_trie = NULL; 

Values global_value= NULL;
int global_count = 0;
int global_uniq_count = 0;
// int global_transitions = 0;
time_t global_t1 ;
bool global_reversed  = false ;
bool global_countable = true ;

/*******************************************************/

/* LOCAL */
Values addValue(Values val,  const char* s){
  Values n_val    = (Values) malloc(sizeof(Values)) ;
  n_val -> string = s   ;
  n_val -> next   = val ;
  return n_val ;
}

/* EXPORT */

int getNumber(const char* str){
  int pos;
 
  for(pos = 0 ; pos < strlen(str); pos++){
    if(str[pos] == '\"' && isdigit (str[pos+1]))
       break;
  }
    size_t size   = strspn(&str[pos+1],"1234567890") ;
    char* number = malloc(sizeof(char)*size);
    memcpy(number,&str[pos+1],size);
    return atoi(number);
}

/*
int getNumber(const char* str){
  char* tmp; 
  strcpy(tmp,str);
  tmp = strreverse(tmp) ;
  tmp = strchr(tmp,'\"');
  size_t size  = strspn(++tmp,"1234567890");
  char*  tmp2  = malloc(sizeof(char)*size);
  memcpy(tmp2, tmp, size);
  tmp = strreverse(tmp2) ;
  return atoi(tmp2);
}
*/

/*  public domain by Bob Stout */
char *strreverse(char *str)
{
      char *p1, *p2;

      if (! str || ! *str)
            return str;
      for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2)
      {
            *p1 ^= *p2;
            *p2 ^= *p1;
            *p1 ^= *p2;
      }
      return str;
}



/* EXPORT */
char* next(){
    char* result ;
  if(more()) {
    result       = (char*) global_value -> string ;
    global_value = global_value -> next ;
    return result ;
  }
  else return "";
}

/* EXPORT */
bool more(){
  return global_value != NULL;
}

/*****************  List functions *************************************/

/* LOCAL */
List addList(List list, Trie trie, char c){
  List new = malloc(sizeof(List));
  new -> c = c;
  new -> trie = trie;
  new -> next = list;
  return new;
}

/* LOCAL */
Trie lookupList(List list, char cl){
  List list_traversal = list;
  while(1){
    if(list_traversal == NULL){
      return NULL;
    }
    else{
      if((list_traversal -> c) == cl)
	return list_traversal -> trie;
      else
	list_traversal = list_traversal -> next;
    }
  }
}

/*****************  Trie functions *************************************/

/* LOCAL */
Trie newTrie(){
  Trie trie;
  trie = (Trie) malloc(sizeof(Trie));
  trie -> list = NULL;
  trie -> value = NULL;
  return trie;
}

/* LOCAL */
Trie add(Trie trie, char c){
  List l;
  Trie t = lookupList(trie -> list,c);
  if (t != NULL) return t;
  else{
    // global_transitions++ ;
    t = newTrie();
    l = addList(trie -> list,t,c);
    trie -> list = l;
    return t;
  }
}

/* LOCAL */
Trie move(Trie trie, char c){
  return lookupList(trie -> list,c);
}

void reversed(){
  global_reversed = true ;
}

void no_count(){
  global_countable = false ;
}

/* LOCAL */
void insert(Trie trie, const char *word,const char* result){
  Trie traversal_ptr;
  char *result_copy;
  int index;
  bool b = false;

  result_copy = malloc(sizeof(char)*(strlen(result)+1));
  strcpy(result_copy,result);

  traversal_ptr = trie;

  if(global_reversed){
    for(index=strlen(word) ; index >= 0 ; index--){
      traversal_ptr = add(traversal_ptr,word[index]);
    }
  }
  else{
    for(index=0 ; index < strlen(word) ; index++){
      traversal_ptr = add(traversal_ptr,word[index]);
    }
  }

  if (traversal_ptr -> value == NULL) /* no previous value at node */ 
   if(global_countable)  
    global_uniq_count++;
  traversal_ptr -> value = addValue(traversal_ptr -> value,result_copy);
  if(global_countable)
    global_count++ ;
  if(global_count % DOT == 0) 
    fprintf(stderr,".");
}

/* GLOBAL */
void insert_t(const char *word, const char* result){
  insert(global_trie, word, result) ;
}

/* LOCAL */
Values accept(Trie trie, const char *word){
  Trie traversal_ptr = trie;
  int index;
  /* Traverse the word */
  if(global_reversed){
    for(index=strlen(word) ; index >= 0 ; index--){
      traversal_ptr = move(traversal_ptr, word[index]);
      if(traversal_ptr == NULL) 
	return NULL;
    }
  }
  else{
    for(index=0 ; index < strlen(word) ; index++){
      traversal_ptr = move(traversal_ptr, word[index]);
      if(traversal_ptr == NULL) 
	return NULL;
    }
  }
  /* Final state reached */
  return traversal_ptr -> value;
}

/************* File operations ************************************/

/* LOCAL: Splits a line into two, with the delimiter DELIMITER */
int split(const char *line, char *fst, char *snd){
  int i;
  char*tmp;
  tmp = strchr(line,DELIMITER);
  tmp++;
  strcpy(snd,tmp);
  for(i=0; i <= MAXLINE ; i++){
    if(line[i] != DELIMITER)
      fst[i] = line[i];
    else
      if(line[i+1] == '{')
        {
          fst[i] = '\0';
          return 1;
        }
  }
  return 0;
}

/* EXPORT */
void empty(){
  global_trie = newTrie();
}

void start(){
  // if(global_reversed)
  //   fprintf(stderr, "reversed ");
  fprintf(stderr, "computing ('.' = 20k word forms): ");
  time(&global_t1);
}

void stop(){
  time_t t2;
  time(&t2);
  fprintf(stderr,"\n\n%dk word forms ", llround(global_count/1000));
  fprintf(stderr,"(c: %d, u: %d)\n", global_count,global_uniq_count);
  // fprintf(stderr,"Trie size (transitions): %.2e\n", (double)global_transitions);
  fprintf(stderr,"compile time: %.2f seconds\n", difftime(t2,global_t1));
}

/* EXPORT */
void build(const char *filename){
  FILE *file_ptr;
  char line[MAXLINE];
  char fst[MAXLINE];
  char snd[MAXLINE];
  char* status;
  char* p;
  start();
  // fprintf(stderr,"Lexicon file: %s\n", filename);
  global_trie = newTrie();
  file_ptr = fopen(filename,"r");
  while(1){    
    status = fgets(line, MAXLINE,file_ptr);
    if(status == NULL)
      break;
    if ((p = strchr(line, '\n')) != NULL)
      *p = '\0';
    if(split(line,fst,snd))
      if(strlen(snd) > 0 || ACCEPT_ZERO)
        insert(global_trie,fst,snd) ;
  }
  stop();
}

/* EXPORT */
void lookup_t(const char* word){
  global_value = accept(global_trie,word);
}

/* EXPORT */
bool in_t(const char* word){
  Values val;  
  val = accept(global_trie,word);
  return val != NULL;
}

/* UNALLOCATE Memory */

/* LOCAL */
void freeValues(Values val){
  if(val == NULL)
    return;
  freeValues(val -> next);
  free(val);
}


void freeTrie(Trie trie) ;

/* LOCAL */
void freeList(List list){
  if(list == NULL)
    return;
  freeTrie(list -> trie);
  freeList(list -> next);
}

/* LOCAL */
void freeTrie(Trie trie){
  if(trie == NULL)
    return;
  freeValues(trie -> value);
  freeList(trie -> list);
  free(trie);
}

/* EXPORT */
void free_trie(){
  freeTrie(global_trie);
  freeValues(global_value);
}
