#include "Interface.h"
#include <iostream>
#include <boost/regex.hpp>

class Console {

    Operable * oprb;

    enum CMD_TYPE { CT_LS, CT_CD, CT_QT, CT_EX, CT_UNDEF, CT_ERR };
    bool dispatch(CMD_TYPE,boost::smatch &);
    CMD_TYPE cmdFilter(std::string,boost::smatch &);

    public:
    Console(Operable &);
    void start();
    void helper();
};
