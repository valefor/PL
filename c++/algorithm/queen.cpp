/*
 * r - row, c - column
 *r/c 1 2 3 4 5 6
 *  1 - x - - - - 
 *  2 - - - x - - 
 *  3 - - - - - x 
 *  4 x - - - - - 
 *  5 - - x - - - 
 *  6 - - - - x - 
 *
 * */

#include <iostream>
#include <stdlib.h>

class Chessboard {
    int dimension;
    int cnt_solution;
    int ** result;
    // Markers
    bool * row, * xl, *xr;

    public:
    Chessboard(int dim) : dimension(dim),cnt_solution(0) {
        row = (bool *)malloc( sizeof(bool)*dimension );
        xl = (bool *)malloc( sizeof(bool) * (dimension*2 -1) );
        xr = (bool *)malloc( sizeof(bool) * (dimension*2 -1) );
        result = (int **)malloc( sizeof(int) * dimension * dimension );

        // Init makers
        row[0] = xl[0] = xr[0] = true;
        for (int i = 1; i < dimension ; i++) {
            row[i] = xl[i] = xr[i] = true;
            xl[i+dimension-1] = xr[i+dimension-1] = true;
        }
    }

    ~Chessboard() {
        free(row);
        free(xl);
        free(xr);
        free(result);
    }

    void solve(int r);
    void print();

    private:
    bool isAvailable(int r, int c) {
        return row[c-1] && xr[c-1+r-1] && xl[c-1+dimension-r];
    }

    void mark(int r, int c, bool reset=false) {
        row[c-1] = reset;
        xr[c-1+r-1] = reset;
        xl[c-1+dimension-r] = reset;
    }
};

void Chessboard::solve(int r) {

    if (r == dimension+1) {
        cnt_solution ++;
        for (int i = 1; i <= dimension ; i++) {
        result[cnt_solution][i] = result[cnt_solution-1][i];
        }
    }

    for (int i = 1; i <= dimension ; i++) {
        row[r] = i;
        int temp = r - dimension;
        if (temp < 0 ) temp = 0 - temp;
        if ( isAvailable(i,r) ) {
            mark(i,r);
            result[cnt_solution][r] = i;
            // Deep First Search
            solve(r+1);
            mark(i,r,true);
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
        for (int i = 0; i < dimension ; i++) {
            std::cout << result[cnt_solution][i] << " ";
        }
        std::cout << std::endl;
    }
}

int main(int argn, char ** args) {

    int dimension;
    //std::cout << argn << std::endl;
    //std::cout << args[1] << std::endl;
    //std::cout << "Enter dimension:" << std::endl;
    // std::cin >> dimension;

    Chessboard * queen = new Chessboard(6);

    queen->solve(1);
    queen->print();
    
    delete queen;
}

