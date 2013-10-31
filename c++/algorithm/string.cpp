/* 
 * string operate algorithm
 * C language
 */
#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <string>
#include <string.h>

int length(const char *);

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
//  here comparing s[j+1] and t[i] saves about 'j' times of meanless comparison
//  we need to evaluate the maximum 'j',
//
//
// high performance version
int* kmp(const char * src, int n) {

    int * next = (int*)malloc(sizeof(int)*n);

    next[0] = -1;
    int i = 0, j = -1;
    while ( i < n-1 ) {
        if ( j == -1 || src[j] == src[i]) {
            // s[0...k-1] == s[j-k...j-1]
            j++;
            i++;
            if (src[j] != src[i])
                next[i] = j;
            else next[i] = next[j];
        } else {
            j = next[j];
        }
    }

    /*
    for (int i = 0; i < n; i++ )
        std::cout << next[i] << " ";
    std::cout << std::endl;
    */

    return next;
}

int kmpFind(const char * target, const char * substr, int * startIdx){
    
    int targetLen = length(target);
    int subLen = length(substr);

    if (*startIdx >= targetLen || subLen < 1 || *startIdx < 0) return -1;

    int * nextv = kmp(substr, subLen);
    int i = *startIdx, j = 0, ret = -1;
    const char * pt = target, *ps = substr;
    while (i <= (targetLen-subLen)) {
        
        if (pt[i] == ps[j]) {
            i++, j++;
        } else {
            if (nextv[j] == -1) {
                i++, j = 0;
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

int length(const char * str) {

    const char * p = str ;
    int i = 0;

    while ( *(p+i) != '\0') {
        i++;
    }
    return i; 
}

// palindromic question
//
// the strings like this "aba" or "ababa"
//
// return the longest palindromic string of
// given string, return NULL if not find
//
// complexity O(n^3) because of 3 for/while loops
char * palindromen3(const char * str) {

    int len = length(str);
    const char * p = str;
    int count = 0, max = count; 
    
    for (int k = 0; k < len; k++) {
        const char * temp = str + k;
        int i = k,j = len -1;
        count = 0;
        while (i < j) {
    
            if (str[i] == str[j]) {
                count +=2;
                i++, j--;
            } else {
                if (i==k) {
                    j--;
                } else {
                    i=k;
                    count = 0;
                }
            }
        }
        if (i==j) count++;
        if (count > max) {
            max = count;
            p = temp;
    std::cout << "Len:" << len << ", Max:" << max << ", Pos:" << k << std::endl;
        }
    }

    char * ret = (char *) malloc(sizeof(char)*(max+1));

    strncpy(ret, p, max);
    ret[max] = '\0';

    return ret;

}

// Here is a palindrome algorithm with complexity O(n)!!
// Don't use this algorithm in interview because you are not expected to
// take it out in that short interview time ...
// How dose it work? Use symmetric property to reduce repeated computations
// supose we have a string: abababac
// we construct a new string by adding a char '#' beside each char, then calculate an
// array P[], for each i in T[i], P[i] means the longest expand of T[i] concentrated 
// palindrome on right hand. something like this
//  S =   a   b   a   b   a   b   a   c
//  T = ^ # a # b # a # b # a # b # a # c # $
//  P = 0 0 1 0 3 0 5 0 7 0 5 0 3 0 1 0 1 0 0
//
//      0 1 2 3 4 5 6 7 8 9 
//                        1-0 1 2 3 4 5 6 7 8
//
//  Ref: http://leetcode.com/2011/11/longest-palindromic-substring-part-ii.html
//
std::string preProcess(std::string str) {
    int n = str.length();
    // add a '^' as begin and a '$' as end of the string
    if (n == 0) return "^$";
    std::string ret = "^";
    for (int i = 0; i < n; i++) ret += "#" + str.substr(i,1);
    ret += "#$";
    
    std::cout << "T:" << ret << std::endl;
    return ret;
}

std::string palindromen(std::string str) {

    // construct string T
    std::string T = preProcess(str);
    int n = T.length();
    int *P = new int[n];
    int c = 1;
    P[0] = P[n-1] = 0;
    P[c] = P[n-1-c] = 0;

    // calculate array P
    for (int i = 2; i < n-2; i++) {
        int R = c + P[c];
        int i_mirror = 2 * c - i;
        if ( R > i ) {
            if (R - i > P[i_mirror]) {
                P[i] = P[i_mirror];
            } else { // P[i] >= R-i, then continue ...
                P[i] = R-i;
            }
        } else {
            P[i] = 0;
        }

        // If the palindrome centered at i does expand past R, 
        // we update C to i, (the center of this new palindrome), 
        // and extend R to the new palindrome’s right edge.
        while (T[i+P[i]+1] == T[i-P[i]-1]) P[i]++;
        if (i+P[i] > R) c = i;
    }

    // Ok, all thing's ready, calculate longest palindrome
    int max = 0;
    int pos = 0;
    for (int j = 0; j < n; j ++) {
        if (max < P[j]) pos = j,max = P[j];
        //std::cout << P[j] << " ";
    }
    //std::cout << std::endl;

    //std::cout << "Pos:" << pos << std::endl;
    //std::cout << "Palin:" << max << std::endl;

    delete[] P;

    // (pos+1)/2 -1)- max/2 == (pos-1-max)/2
    return str.substr(((pos+1)/2 -1)- max/2, max);
    
}

int main () {
    /*
    char * target = "The tttts tetest test string test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test test";
    char * substr = "test";

    int pos = 0, startIdx = 0;
    while ( (pos = kmpFind(target,substr,&startIdx)) > 0) {
        std::cout << pos << std::endl;
    }

    std::ifstream file;
    std::string line;
    int lineNum = 1;
    file.open("string.cpp");
    
    // use std getline to read one line each time
    // or use '>>' to read a word each time
    while (std::getline(file, line)) {
        pos = 0,startIdx = 0;
        std::cout << "LINE:" << line << std::endl;
        while ( (pos = kmpFind(line.c_str(),substr,&startIdx)) > 0) {
            std::cout << "Line:" << lineNum << ", Pos:" <<pos << std::endl;
        }
        lineNum ++;
    }
    */
    std::string input;
    std::cout << "Please enter a string:" << std::endl;
    std::cin >> input;
    
    //std::cout << palindromen3(input.c_str()) << std::endl;
    //std::cout << palindromen("abababac") << std::endl;
    std::string palin = palindromen(input);
    std::cout << palin << std::endl;
}
/* Find the longest palindrome of the following long string ;-)
FourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnationconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequalNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth
eforthepeopleshallnotperishfromthee
*/

