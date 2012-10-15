#include "Interface.h"
#include <iostream>

class Console {

    Operable * oprb;

    public:
    Console(Operable &);
    void start();
    void helper();
    int dispatch(std::string,std::string);
};
