

// simple sort
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
    
        temp = array[i];
        for (j = i ; j > start ; j--) {
            if (array[j] > temp) array[j+1] = array[j];
        }
        array[j+1] = temp;
    }
}

// pointer version
void insertSortP(int* array, int n) {

}

// seperate sorted from unsorted elements
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

// Advance sort
void quickSort(int* array, int start, int end) {


}

