/* on.cpp
 *
 * By William Ersing
 *
 * CodeEval challenge (EASY) Odd Numbers
 * https://www.codeeval.com/open_challenges/25/
 */

#include <iostream>
using namespace std;

int main(void){
    for(size_t i = 1; i < 100; i++) {
        if((i % 2) != 0) {
            cout << i << endl;
        }
    }
}
