/*
 * The main test routine
 *
 */
#include "list.h"
#include "ptr.h"
#include <iostream>

int main(int argn, char ** argv) {

    List<int> l;
    l.append(1).append(2).append(3).append(4);
    l.reverse();
    l.print();
}
