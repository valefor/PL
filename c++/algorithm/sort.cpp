

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

void fastSort
