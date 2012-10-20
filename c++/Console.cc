#include "Console.h"
#include <string>

Console::Console(Operable & opra){
    root = oprb = &opra;
}  

void Console::start() {

    oprb->show();

    std::string cmd = "";
    boost::smatch matches;

    while (true)
    {
        std::cout << ">:";
        std::getline(std::cin,cmd);
        CMD_TYPE ct = cmdFilter(cmd,matches);
        

        if (!dispatch(ct,matches)) break;

    }
}

Console::CMD_TYPE Console::cmdFilter(std::string cmd,boost::smatch & sm) {
    //std::cout << "command:" << cmd << "\n";
    boost::regex lsRegex("(ls|show|s)[ \t]*(.*)");
    boost::regex cdRegex("(goto|g|cd)[ \t]*(.*)");
    boost::regex qtRegex("(quit|q|exit)[ \t]*(.*)");
    boost::regex excRegex("(ex|execute)[ \t]*(.*)");
    boost::regex helpRegex("(help|h|\\?)[ \t]*(.*)");
    boost::regex undefRegex("([a-zA-Z]+)[ \t]*(.*)");
    
    if(boost::regex_match(cmd,sm,qtRegex)) return CT_QT;
    if(boost::regex_match(cmd,sm,helpRegex)) return CT_HP;
    if(boost::regex_match(cmd,sm,lsRegex)) return CT_LS;
    if(boost::regex_match(cmd,sm,excRegex)) return CT_EXC;
    if(boost::regex_match(cmd,sm,cdRegex)) return CT_CD;
    if(boost::regex_match(cmd,sm,undefRegex)) return CT_UNDEF;

    return CT_ERR;
}

bool Console::dispatch(CMD_TYPE cmdType,boost::smatch & sm) {

    //std::cout << "CMD_TYPE:" << cmdType << "\n";
    Operable * opr;
    switch(cmdType) {
      case CT_QT: 
        return false;
      case CT_CD:
        if(sm.str(2).empty()) {
          std::cout << "command:" << sm.str(1) << " needs arguments!" << "\n";
        } else {
          const char * arg= sm.str(2).c_str();
          if(std::atoi(arg)) {
            if( ( opr = oprb->cd(std::atoi(arg)) ) ) {
                oprb = opr;
            }
            else {
                std::cout << "Cann't find item with index:" << sm.str(2) << "\n";
            }
          } else if(sm.str(2) == "..") {
            if(oprb->getParent()) oprb = oprb->getParent();
            else std::cout << "Have reached root!" << "\n";
          } else if(sm.str(2) == "/") {
            oprb = root;
          } else {
            std::cout << "Invalid argument:" << sm.str(2) << "\n";
          }
        }
        
        break;
      case CT_LS:
        if(sm.str(2).empty()) {
          oprb->show();
        } else {
          const char * arg= sm.str(2).c_str();
          if(std::atoi(arg)) {
            if(!oprb->show(std::atoi(arg))) 
              std::cout << "Cann't show item with index:" << sm.str(2) << "\n";
          } else {
            std::cout << "Invalid argument:" << sm.str(2) << ",only accept digits\n";
          }
        }
        break;
      case CT_EXC:
        oprb->execute();
        break;
      case CT_UNDEF:
        std::cout << "Unrecognized command:" << sm.str(1) <<"\n";
      case CT_HP:
        helper();
        break;
      default:
        ;
    }

    return true;
}

void Console::helper() {
    
    std::cout << "Usage: command [args...]:\n"
        << "    cd <index>  Enter the sub directory with index,\n"
        << "                'goto' or 'g' are candidates,about <index>:\n"
        << "                '/' means root dir,'..' means parent dir\n"
        << "\n"
        << "    ls [index]  show information of current content,\n"
        << "                argument 'index' is optional,only accept digits.\n"
        << "                'show' or 'l' are candidates.\n"
        << "\n"
        << "    ex          execute current content if it's executable,\n"
        << "                'execute' or 'e' are candidates.\n"
        << "\n"
        << "    quit        quit this program,'exit' or 'q' are candidates.\n"
        << "\n"
        << "    help        show help message,'h' and '?' are abbrevs.\n";
}
