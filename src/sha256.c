/* #include "idris_rts.h" */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/sha.h>
#include "sha256.h"

char *inc(char *str) // , char outputBuffer[65])
{
  unsigned char *d = SHA256(str, strlen(str), 0);
  int i;

  char *outputBuffer = malloc(65);

  for (i = 0; i < SHA256_DIGEST_LENGTH; i++)
    sprintf(outputBuffer + (i * 2), "%02x", d[i]);
    /* printf("%02x", d[i]); */
  return outputBuffer;
}

    /* int i = 0; */
    /* for(i = 0; i < SHA256_DIGEST_LENGTH; i++) */
    /* { */
    /*     sprintf(outputBuffer + (i * 2), "%02x", hash[i]); */
    /* } */
    /* outputBuffer[64] = 0; */
 
/* void sha (char *s) { */
/* 	/\* const char *s = "[\"0xaa5598c670c7f9c9ab8594d942390dad96b52631bfb78d060476b9a719365947\",[{\"inputs\":[{\"amount\":7,\"id\":2}],\"outputs\":[{\"amount\":4,\"id\":3},{\"amount\":3,\"id\":4}]},{\"inputs\":[{\"amount\":15,\"id\":13},{\"amount\":15,\"id\":34}],\"outputs\":[{\"amount\":30,\"id\":73}]}],3,1899]"; *\/ */
 
/* 	int i; */
/* 	for (i = 0; i < SHA256_DIGEST_LENGTH; i++) */
/* 		printf("%02x", d[i]); */
/* 	putchar('\n'); */
/* } */
