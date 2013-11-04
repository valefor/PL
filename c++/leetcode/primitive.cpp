/*
 *  This file contains algorithm of primitives such as number, string ,char. etc
 */


// Let's start
#include <iostream>
#include <string>
#include <stdio.h>

/*
 * Palindrome number
 *
 * Des:
 *  Determine whether an integer is a palindrome
 *
 * Requirements:
 *  Resolve this problem without extra space
 */
bool isPalindrome(int num) {
    if (num < 0) return false;

    // calculate length of number?
    int div = 1;
    while (num/div >= 10) {
        div *= 10;
    }

    while (num > 10) {
        int l = num/div;
        int r = num%10;
        if (l!=r) return false;
        // chop start and end of number
        num = (num%div)/10;
        div /= 100; 
    }

    return true;
}

/*
 * Regular Expression Matching
 *
 * Des:
 *  '.' Matches any single character.
 *  '*' Matches zero or more of the preceding element.
 *
 */
bool isMatch(const char * str, const char * reg) {
    int i = 0,j = 0;
    bool wildcard = false;

    if (str[0]=='\0' && reg[0]=='\0') return true;

    while (str[i] != '\0') {
        if (reg[j+1] == '*') wildcard = true;
        else wildcard = false;

        if (str[i] == reg[j]) {
            if (!wildcard) j++;
        } else {
            if (reg[j] != '.') {
                if (!wildcard) return false;
                else j += 2, i--;
            }
            if (!wildcard) j++;
        }

        i++;
    }
    return true;  
}

// Invert a integer in this way:
//  Give a integer 3, it's "00000000 00000000 00000000 00000011" in binary format
//  invert it : "11000000 00000000 00000000 00000000", supose we are on 32bit system
#define CHAR_BIT 8
unsigned int invert(unsigned int num) {
    /*
    int ret = 0;
    int bit = 0;

    
    while (bit < 32) {
        if (num & (1 << bit))
        ret |= 1 << 31-bit;
        bit ++ ;
    }
    */

    // more graceful algorithm!
    int ret = num;
    // steps count
    int s = sizeof(num) * CHAR_BIT - 1;
    for (num >>= 1; num > 0; num >>= 1) {
        ret <<= 1;
        ret |= num & 1;
        s --;
    }
    ret <<= s;

    printf("number:%x, invert: %x\n", num, ret);

    return ret;
}

// And two more bit operating tricks


// Double Square Problem
//  Des:
//  A double-square number is an integer X which can be expressed as the sum of 
//  two perfect squares. For example, 10 is a double-square because 10 = 32 + 12. 
//  Your task in this problem is, given X, determine the number of ways in which 
//  it can be written as the sum of two squares.
int doubleSquare(unsigned int num) {
    
}

// test
int main(void) {
    int num= 23332;
    std::cout << num <<  " is a palindrome?" << std::boolalpha << isPalindrome(num) << std::endl;
    std::cout << "aa and \"a\" is match?" << std::boolalpha << isMatch("aa","a") << std::endl;
    std::cout << "aa and \"a*\" is match?" << std::boolalpha << isMatch("aa","a*") << std::endl;
    std::cout << "aa and \"aa\" is match?" << std::boolalpha << isMatch("aa","aa") << std::endl;
    std::cout << "aaa and \"aa\" is match?" << std::boolalpha << isMatch("aaa","aa") << std::endl;
    std::cout << "aab and \".*\" is match?" << std::boolalpha << isMatch("aab",".*") << std::endl;
    std::cout << "aab and \"c*a*b*\" is match?" << std::boolalpha << isMatch("aab","c*a*b*") << std::endl;

    std::cout << "Invert of integer(364) is:" << invert(364) << std::endl; 
}
