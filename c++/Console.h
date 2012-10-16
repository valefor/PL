#include "Interface.h"
#include <iostream>

class Console {

    Operable * oprb;

    enum CMD_TYPE { CT_LS,CT_CD,CT_QT,CT_EX, CT_UNDEF };
    bool dispatch(CMD_TYPE,std::string);
    CMD_TYPE cmdFilter(std::string);

    public:
    Console(Operable &);
    void start();
    void helper();
};
