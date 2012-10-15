#include "Interface.h"

class Console {

    Operable * oprb;

    public:
    Console(Operable & opra);
    void start();
    void helper();
};
