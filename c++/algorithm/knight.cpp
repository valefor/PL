/*
 *  N knight question, BFS(Breadth First Search) Algorithm
 *
 *  <<KNIGHT>>
 *
 * x - knight, o - attack point
 *
 *  N6:
 *r/c 1 2 3 4 5 6
 *  1 x o o o o x
 *  2 o o o o o o 
 *  3 o o x x o o 
 *  4 o o x x o o 
 *  5 o o o o o o 
 *  6 x o o o o x
 *
 *r/c 1 2 3 4 5 6
 *  1 - - - - - -
 *  2 - - - - - -
 *  3 - - - - - -
 *  4 - - - - - -
 *  5 - - - - - -
 *  6 - - - - - -
 *
 * */

#include <iostream>
#include <vector>

class Point {
    int x,y;

    public:
    explicit Point(int a, int b):x(a),y(b) {}
};

class Chessboard {
    int number;
    int cnt_knight;
    int cnt_occupied;
    char ** matrix;
    // The knight can attack in 8 directions in maximum
    std::vector<Point> points;

    public:
    Chessboard(int n) : number(n),cnt_knight(0),cnt_occupied(0) {
        matrix = NULL;
        initMatrix(number);
    }

    ~Chessboard() {
        delete [] matrix;
    }

    void solve(int r);
    void print();

    private:

    bool isOccupied(int r, int c) {
        return matrix[r][c] == 'x';
    } 

    bool isCoverred(int r, int c) {
        return matrix[r][c] == 'o';
    }

    bool isAvailable(int r, int c) {
        return !isOccupied(r,c) && !isCoverred(r,c);
    }

    int maxAttackPoints(int r, int c) {
        int count = 0;
        if ( r-2 >= 0 && c-1 >= 0 && isAvailable(r-2,c-1) ) count++;
        if ( r-2 >= 0 && c+1 < number && isAvailable(r-2,c+1) ) count++;
        if ( r-1 >= 0 && c-2 >= 0 && isAvailable(r-1,c-2) ) count++;
        if ( r-1 >= 0 && c+2 < number && isAvailable(r-1,c+2) ) count++;

        if ( r+2 < number && c-1 >= 0 && isAvailable(r+2,c-1) ) count++;
        if ( r+2 < number && c+1 < number && isAvailable(r+2,c+1) ) count++;
        if ( r+1 < number && c-2 >= 0 && isAvailable(r+1,c-2) ) count++;
        if ( r+1 < number && c+2 < number && isAvailable(r+1,c+2) ) count++;

        return count;
    }

    void mark(int r, int c) {
        matrix[r][c] = 'x', cnt_occupied ++;

        if ( r-2 >= 0 && c-1 >= 0 ) matrix[r-2][c-1]='o', cnt_occupied ++;
        if ( r-2 >= 0 && c+1 < number ) matrix[r-2][c+1]='o', cnt_occupied ++;
        if ( r-1 >= 0 && c-2 >= 0 ) matrix[r-1][c-2]='o', cnt_occupied ++;
        if ( r-1 >= 0 && c+2 < number ) matrix[r-1][c+2]='o', cnt_occupied ++;

        if ( r+2 < number && c-1 >= 0 ) matrix[r+2][c-1]='o', cnt_occupied ++;
        if ( r+2 < number && c+1 < number ) matrix[r+2][c+1]='o', cnt_occupied ++;
        if ( r+1 < number && c-2 >= 0 ) matrix[r+1][c-2]='o', cnt_occupied ++;
        if ( r+1 < number && c+2 < number ) matrix[r+1][c+2]='o', cnt_occupied ++;
    }

    int generatePoints(int r, int c) {
        if ( r-2 >= 0 && c-1 >= 0 ) points.push_back(Point(r-2,c-1));
        if ( r-2 >= 0 && c+1 < number ) points.push_back(Point(r-1,c+1));
        if ( r-1 >= 0 && c-2 >= 0 ) points.push_back(Point(r-1,c-2));
        if ( r-1 >= 0 && c+2 < number ) points.push_back(Point(r-1,c+2));

        if ( r+2 < number && c-1 >= 0 ) points.push_back(Point(r+2,c-1));
        if ( r+2 < number && c+1 < number ) points.push_back(Point(r+2,c+1));
        if ( r+1 < number && c-2 >= 0 ) points.push_back(Point(r+1,c-2));
        if ( r+1 < number && c+2 < number ) points.push_back(Point(r+1,c+2));

        return points.size();
    }

    void initMatrix(int n) {
        char ** matrix = new char*[n*n];
        matrix[0] = new int[n*n];
        for (int j = 0; j < n; j++) {
            matrix[0][j] = '-';
        }
        for (int i =1; i< n; i++) {
            result_new[i] = result_new[i-1]+ n;
            for (int j = 0; j < n; j++) {
                matrix[i][j] = '-';
            }
        }
    }
};

void Chessboard::solve() {
    int max = 0;
    int x,y;

    while (cnt_occupied < number*number ) {

    for (int i = 0 ; i < number ; i++)
    {
        for (int j = 0; j < number ; j++) {
            if (!isAvailable(i,j)) continue;
            int cntPoints =  maxAttackPoints(i,j);
            if ( max < cntPoints )  x=i, y=j, max = cntPoints;
            if ( max = 8 ) break;
        }
        if ( max = 8 ) break;
    }
    mark(x,y);
    
    }
}

void Chessboard::print() {

    if (cnt_solution==0) 
    {
        std::cout << "No solution has been found" << std::endl;
        return;
    }

    for (int  i = 0 ; i < number ; i++)
    {
        for (int j = 0; j < number ; j++) {
            std::cout << matrix[i][j] << " ";
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
