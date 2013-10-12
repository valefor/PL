/*
 *  N queen question, DFS(Depth First Search) Algorithm
 *
 * r - row, c - column
 *
 *  <<QUEEN>>
 *
 *  N6:
 *r/c 1 2 3 4 5 6
 *  1 - x - - - -
 *  2 - - - x - - 
 *  3 - - - - - x 
 *  4 x - - - - - 
 *  5 - - x - - - 
 *  6 - - - - x - 
 *
 *  N7:
 *r/c 1 2 3 4 5 6 7
 *  1 x - - - - - -
 *  2 - - x - - - - 
 *  3 - - - - x - - 
 *  4 - - - - - - x 
 *  5 - x - - - - - 
 *  6 - - - x - - - 
 *  7 - - - - - x - 
 *
 *  <<KNIGHT>>
 *
 *  N6:
 *r/c 1 2 3 4 5 6
 *  1 o o o o - -
 *  2 x x o o - - 
 *  3 o x x o - - 
 *  4 o - o o - - 
 *  5 o - o - - - 
 *  6 - - - - - - 
 *
 * */

#include <iostream>
#include <stdlib.h>

class Chessboard {
    int number;
    int cnt_solution;
    int ** result;
    int result_size;
    // Markers
    bool * row, * xl, *xr;

    public:
    Chessboard(int n) : number(n),cnt_solution(0) {
        //row = (bool *)malloc( sizeof(bool)*number );
        //xl = (bool *)malloc( sizeof(bool) * (number*2 -1) );
        //xr = (bool *)malloc( sizeof(bool) * (number*2 -1) );
        //result = (int **)malloc( sizeof(int) * number * number );
        row = new bool[number];
        xl = new bool[number*2-1];
        xr = new bool[number*2-1];
        result = NULL;
        resizeResult(number);

        // Init makers
        row[0] = xl[0] = xr[0] = true;
        for (int i = 1; i < number ; i++) {
            row[i] = xl[i] = xr[i] = true;
            xl[i+number-1] = xr[i+number-1] = true;
        }
    }

    ~Chessboard() {
        delete [] row;
        delete [] xl;
        delete [] xr;
        delete [] result;
    }

    void solve(int r);
    void print();

    private:
    bool isAvailable(int r, int c) {
        /*
        std::cout << "row:" << r << "column:" << c << std::endl;
        std::cout << "xr:" << c-1+r-1 << "[" << xr[c-1+r-1] << "]" << std::endl;
        std::cout << "xl:" << c-1+number-r << "[" << xr[c-1+number-r] << "]" << std::endl;
        if (row[c-1] && xr[c-1+r-1] && xl[c-1+number-r]) {
            std::cout << "available!" << std::endl;
        }
        */
        return row[c-1] && xr[c-1+r-1] && xl[c-1+number-r];
    }

    void resizeResult(int n) {
        int old_size = result_size;

        result_size = n*n+1;
        int ** result_new = new int*[result_size*number];
        result_new[0] = new int[result_size*n];
        for (int i =1; i< result_size; i++) result_new[i] = result_new[i-1]+ number;

        if(result != NULL) {
            for (int i = 0; i < old_size; i++ ) result_new[i] = result[i];
        }
        delete [] result;
        result = result_new;
    }

    void xxDebug() {
        std::cout << "***XR-XL INFO BEGIN***" << std::endl;
        
        std::cout << "row[" ;
        for ( int i = 0; i< number; i++ ) {
            std::cout << row[i] << " ";
        }
        std::cout << "]" << std::endl;   
        std::cout << "xr[" ;
        for ( int i = 0; i< number*2-1; i++ ) {
            std::cout << xr[i] << " ";
        }
        std::cout << "]" << std::endl;
        std::cout << "xl[";
        for ( int i = 0; i< number*2-1; i++ ) {
            std::cout << xl[i] << " ";
        }
        std::cout << "]" << std::endl;
        std::cout << "***XR-XL INFO END***" << std::endl;
    }

    void mark(int r, int c, bool reset=false) {
        row[c-1] = reset;
        xr[c-1+r-1] = reset;
        xl[c-1+number-r] = reset;
    }
};

void Chessboard::solve(int r) {
    if (r == number+1) {
        cnt_solution ++;
        if (cnt_solution >= result_size)
            resizeResult(result_size);
        for (int i = 0; i < number ; i++) {
            result[cnt_solution][i] = result[0][i];
            //print();
        }
        return;
    }

    for (int i = 1; i <= number ; i++) {
        if ( isAvailable(r,i) ) {
            mark(r,i);
            result[0][r-1] = i;
            // Depth First Search
            solve(r+1);
            mark(r,i,true);
        }
    }
}

void Chessboard::print() {

    if (cnt_solution==0) 
    {
        std::cout << "No solution has been found" << std::endl;
        return;
    }

    for (int  i = 1 ; i <= cnt_solution ; i++)
    {
        for (int j = 0; j < number ; j++) {
            std::cout << result[i][j] << " ";
        }
        std::cout << std::endl;
    }
    std::cout << cnt_solution << std::endl;
}

int main(int argn, char ** args) {

    int number = 0;
    //std::cout << argn << std::endl;
    //std::cout << args[1] << std::endl;
    while(true) {
    std::cout << "Enter number of queens[1-9]:" << std::endl;
    std::cin >> number;
    if ( number > 0 && number < 10 ) break;
    else std::cout << "The number must be shorter than 10 and bigger than 0" << std::endl;
    }
    Chessboard * queen = new Chessboard(number);

    queen->solve(1);
    queen->print();
    
    delete queen;
}

