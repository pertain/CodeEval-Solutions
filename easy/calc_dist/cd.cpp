/* cd.cpp
 *
 * By William Ersing
 *
 * CodeEval challenge (EASY) Calculate Distance
 * https://www.codeeval.com/open_challenges/99/
 */

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
using namespace std;

float CalcDist(int x1, int y1, int x2, int y2){
    return sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2));
}

void ParsePoint(string ln, int &x, int &y){
    size_t delimIndex;

    ln = ln.substr(1, ln.length() - 2);     // remove leading/trailing parentheses
    delimIndex = ln.find(", ");
    x = stoi(ln.substr(0, delimIndex));
    y = stoi(ln.substr(delimIndex + 2));
}

void ParseLine(string ln, int &x1, int &y1, int &x2, int &y2){
    size_t delimIndex;

    delimIndex = ln.find(" (");
    ParsePoint(ln.substr(0, delimIndex), x1, y1);
    ParsePoint(ln.substr(delimIndex + 1, ln.length() - 1), x2, y2);
}

int main(int argc, char** argv){
    int x1, y1;
    int x2, y2;
    string fline;

    if(argc != 2){
        cout << "Invalid Argument (Usage: <command> <filename>)" << endl;
    }
    else {
        ifstream ifs(argv[1]);

        if(!ifs.is_open()){
            cout << "Could not open the file: " << argv[1] << endl;
        }
        else {
            while(getline(ifs, fline)){
                ParseLine(fline, x1, y1, x2, y2);
                cout << CalcDist(x1, y1, x2, y2) << endl;
            }
        }
    }

    return 0;
}
