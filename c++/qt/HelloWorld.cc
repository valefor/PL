#include <qapplication.h>
#include <qwidget.h>
#include <qpushbutton.h>

int main(int argc,char ** argv) {
    QApplication app( argc, argv );

    QPushButton helloWorld( "Hello World!");
    helloWorld.resize( 100, 300 );

    helloWorld.show();
    return app.exec();
}

