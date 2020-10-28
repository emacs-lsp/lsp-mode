#include <iostream>

void check_bigger(int left, int right) {
  if (left < right) {
    std::cout << left << " is smaller than " << right << "\n";
  } else {
    std::cout << left << " at least as big as " << right << "\n";
  }
}
