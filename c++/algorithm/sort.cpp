/*
 * Think about how to improve sort efficiency?
 *
 * reduce search/exchange operations
 *
 *
 */

#include <iostream>

void print(int *, int, int);

/*******************
 *  Simple sort    *
 *******************/
void bubbleSort(int* array, int n) {
    
    for (int i = 0; i < n; i++) {
        bool exchanged = false;
        for (int j = n-1; j > i ; j--) {

            // exchange
            if ( array[j-1] > array[j] ) {
                exchanged = true;
                int temp = array[j-1];
                array[j-1] = array[j];
                array[j] = temp;
                /* XOR version
                array[j-1] = array[j-1]^array[j];
                array[j] = array[j-1]^array[j];
                array[j-1] = array[j-1]^array[j];
                */
            }
        }
        if (!exchanged) break;
    }
}

void insertSort(int* array, int start, int end) {
    int temp;
    int i,j;

    for (i = start+1; i < end; i++) {
    
        temp = array[i-1];
        for (j = i ; j > start ; j--) {
            if (array[j] > temp) array[j+1] = array[j];
        }
        array[j+1] = temp;
    }
}

// pointer version
void insertSortP(int* array, int n) {

}

// Seperate sorted from unsorted elements:
//  Thus, amount of search/exchange operations will be cut down
// The Best choice in simple sort algorithms
void selectSort(int * array, int n) {
    for (int i = 0; i < n; i++) {
        int temp = i;
        for (int j = i+1; j < n ; j++) {
            if (array[temp] > array[j]) {
                temp = j;
        }
        if( temp != i) {
            int t = array[i];
            array[i] = array[temp];
            array[temp] = t;
            }
        }
    }
}

/*******************
 *  Advanced sort  *
 *******************/
// Proved most efficient sort algorithm
void quickSort(int* array, int start, int end) {

    if (start >= end) return;

    int i,j, key;

    i = start;
    j = end;
    key = array[i];
    while ( i < j) {
        if (array[j] < key) {
            array[i] = array[j];
            i++;
            if ( i >= j) break;
            if (array[i] > key) {
                array[j] = array[i];
            }
        }
        j--;
    }

    array[i] = key;

    // at here, i == j
    print(array,start,end);
    quickSort(array, start, i);
    quickSort(array, j+1, end);

}

// Heap sort implementation relies on data structure: complete binary tree
void heapSort() {

}

void print(int * array, int start, int end) {
    for (int i = start; i < end; i++) {
        std::cout << array[i] << " ";
    }
    std::cout << std::endl;
}

int main() {

    int a[] = {1,3,5,7,9,8,6,4,2};
    int n = 9;

    quickSort(a,0,n);
    print(a,0,n);
}
