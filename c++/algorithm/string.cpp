/* 
 * string operate algorithm
 * C language
 */
#include <iostream>
#include <stdlib.h>
int length(char *);

/*
 * @params
 *  target - target string
 *  substr - sub string
 *
 * @return : int
 *  returns the first position where the sub string is found.
 *  -1 means no found 
 */
int find(char * target, char * substr, int * startIdx) {
    // what if startIdx is bigger than length or target string?
    // But if target string is toooo long, calculating the length
    // may reduce performance.
    int targetLen = length(target);
    int subLen = length(substr);
    
    if (*startIdx >= targetLen || subLen < 1 || *startIdx < 0) return -1;
    char * pt = target, *ps = substr;
    int i = *startIdx,j = 0, ret = -1;
    while (i <= (targetLen-subLen)) {

        if (pt[i] == ps[j]) {
            i++, j++;
        } else {
            if (j != 0) {
                i = i - j, j = 0;
            } 
            //std::cout << "pt[" << i << "]"<< pt[i] << ":" << ps[j] << std::endl;
            i ++;
        }

        if (j == subLen) {
            *startIdx = i;
            ret = i-j;
            break;
        }
    }

    return ret;
}

// How to improve the performance?
//  reduce regress steps when mismatch happened 
//  KMP algorithm:
//  Supose t means target string, s means src string,
//  t[i] != s[i], that indicates t[0...i-1] == s[0...i-1]
//  if there is a way to know s[0...j] == s[i-j-1...i-1], when mismatch happened,
//  no need to regress to s[0] to start comparison again, start from s[j+1],
//  here comparing s[j+1] and t[i] save about 'j' times of meanless comparison
//  we need to evaluate the maximum 'j',
//
//
// high performance version
int* kmp(char * src, int n) {

    int * next = (int*)malloc(sizeof(int)*n);

    next[0] = -1;
    //int i = 0;
    for (int i = 0 ; i < n; i++) {
        int max = 0; 
        for (int k = 0; k < i; k++) {
            if (src[k] == src[max]) {
                k++,max++;
            }
        }
        next[i] = max;
    }

    return next;
}

int kmpFind(char * target, char * substr, int * startIdx){
    
    int targetLen = length(target);
    int subLen = length(substr);

    if (*startIdx >= targetLen || subLen < 1 || *startIdx < 0) return -1;

    int * nextv = kmp(substr, subLen);
    int i = *startIdx, j = 0, ret = -1;
    char * pt = target, *ps = substr;
    while (i <= (targetLen-subLen)) {
        
        if (pt[i] == ps[j]) {
            i++, j++;
        } else {
            if (nextv[j] == -1) {
                i++, j == 0;
            } else {
                j = nextv[j];
            }
        }

        if (j == subLen) {
            *startIdx = i;
            ret = i-j;
            break;
        }
    }

    // release nextv before return;
    free(nextv);
    return ret;
}

int length(char * str) {

    char * p = str ;
    int i = 0;

    while ( *(p+i) != '\0') {
        i++;
    }
    return i; 
}

int main () {
    char * target = "The tttts test string test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test";
    char * substr = "test";

    int pos = 0, startIdx = 0;
    while ( (pos = find(target,substr,&startIdx)) > 0) {
        std::cout << pos << std::endl;
    }
}
