/* 
 * string operate algorithm
 * C language
 */
#include <iostream>
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
    char * pt = target+*startIdx, *ps = substr;
    while (*pt != '\0') {
        if (*ps == '\0') return (*startIdx)-subLen;

        if (*pt == *ps) {
            ps++;
        } else {
            ps = substr;
        }
        pt++, (*startIdx)++;
    }
    if (*ps == '\0') return (*startIdx)-subLen;

    return -1;
}

// How to improve the performance?
// 
//
// high performance version
int eFind() {

}

int length(char * target) {

    char * p = target ;
    int i = 0;

    while ( *(p+i) != '\0') {
        i++;
    }
    return i; 
}

int main () {
    char * target = "The test string test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test";
    char * substr = "test";

    int pos = 0, startIdx = 0;
    while ( (pos = find(target,substr,&startIdx)) > 0) {
        std::cout << pos << std::endl;
    }
}
