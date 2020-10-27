#include <iostream>
#include "main.h"

using namespace std;

// defined in individual_file.cpp
void check_bigger(int left, int right);

int multiply_by_seventeen(int param) {
    return param * 17;
}

int main(int argc, char *argv[]) {
    cout << "Wagwan world!" << "\n";

    check_bigger(10, 25);

    return 0;
}
