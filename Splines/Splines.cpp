#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
using namespace std;

struct Function {
    double (*f)(double);
    string fName;
};

Function TestFunc[];

struct Table {
    int Size;
    double A, B;
    double* x;
    double* y;

    Table(Function& fCont, int N, double A, double B, double* X) {
        Size = N;
        this->A = A;
        this->B = B;
        x = new double[N];
        y = new double[N];
        for (int i = 0; i < N; i++) {
            x[i] = X[i];
            y[i] = fCont.f(x[i]);
        }
    }

    Table(int N, double A, double B, double* X, double* Y) {
        Size = N;
        this->A = A;
        this->B = B;
        x = new double[N];
        y = new double[N];
        for (int i = 0; i < N; i++) {
            x[i] = X[i];
            y[i] = Y[i];
        }
    }

    void Display() {
        cout << "A = " << A << " B = " << B << endl;
        cout << " x ";
        for (int i = 0; i < Size; i++) {
            cout << x[i] << " ";
        }
        cout << endl << " y ";
        for (int i = 0; i < Size; i++) {
            cout << y[i] << " ";
        }
        cout << endl;
        cout << endl;
    }

    void hhhTable() {
        delete[] x;
        delete[] y;
    }
};

class Splain {
    double left;
    double right;
public:
    double c[4]; // c[3] must /2 and c[4] must /6

    Splain() {
        left = right = c[0] = c[1] = c[2] = c[3] = 0;
    }

    Splain(double l, double r) {
        left = l;
        right = r;
        for (int i = 0; i < 4; i++) {
            c[i] = 1;
        }
    }

    string ToString() {
        string res = " ";
        double a = c[0];
        double b = c[1];
        double C = c[2] / 2;
        double d = c[3] / 6;

        stringstream str;
        str << " " << d << "*x**3 + " << (C - 3 * d * right) << "*x**2 + " << (b - 2 * C * right + 3 * d * pow(right, 2)) << "*x + " << (a - b * right + C * pow(right, 2) - d * pow(right, 3));
        /* res +=  string(d);
         res += "*x**3 + ";
         res += (C - 3 * d * right);
         res += "*x**2 + ";
         res += (b - 2 *C * right + 3 * d * pow(right, 2));
         res += "*x + ";
         res += (a - b * right + C * pow(right, 2) - d * pow(right, 3));*/
        res = str.str();
        return res;
    }
    friend ostream& operator<<(ostream& out, Splain const& p);
};

ostream& operator<<(ostream& out, Splain const& p)
{
    double a = p.c[0];
    double b = p.c[1];
    double c = p.c[2] / 2;
    double d = p.c[3] / 6;
    cout << " " << d << "*x**3 + " << (c - 3 * d * p.right) << "*x**2 + " << (b - 2 * c * p.right + 3 * d * pow(p.right, 2)) << "*x + " << (a - b * p.right + c * pow(p.right, 2) - d * pow(p.right, 3)) << endl;
    return out;
}
//рассчёт проверен
double* SolveSystem(double* A, double* B, double* C, double* vec, int N) {
    double* res = new double[N];
    double* mu = new double[N];
    double* nu = new double[N];
    mu[0] = vec[0] / A[0];
    nu[0] = -1 * C[0] / A[0];
    for (int i = 1; i < N - 1; i++) {
        mu[i] = (vec[i] - B[i - 1] * mu[i - 1]) / (A[i] + B[i - 1] * nu[i - 1]);
        nu[i] = (-1 * C[i]) / (A[i] + B[i - 1] * nu[i - 1]);
    }
    mu[N - 1] = vec[N - 1] / A[N - 1];
    nu[N - 1] = -1 * (B[N - 2] / A[N - 1]);
    /*for (int i = 0; i < 3; i++) {
        cout << " Deb mu nu " << mu[i] << " " << nu[i] << " " << i << endl;
    }*/
    res[N - 1] = (mu[N - 1] + nu[N - 1] * mu[N - 2]) / (1 - nu[N - 1] * nu[N - 2]);
    for (int i = N - 2; i >= 0; i--) {
        res[i] = mu[i] + nu[i] * res[i + 1];
    }
    return res;
}

Splain* SolveTable(Table table) {
    int numberOfEq = table.Size - 1;
    Splain* res = new Splain[numberOfEq];
    for (int i = 0; i < numberOfEq; i++) {
        res[i] = Splain(table.x[i], table.x[i + 1]);
    }

    double* h = new double[numberOfEq];
    for (int i = 0; i < numberOfEq; i++) {
        h[i] = table.x[i + 1] - table.x[i];
    }
    //double* F = new double[numberOfEq-1]; //доп пустой эл в начале для соотв индексов
    //for (int i = 1; i < numberOfEq-1; i++) {
    //    F[i] = 6 * ((table.y[i + 1] - table.y[i]) / h[i + 1] - (table.y[i] - table.y[i - 1]) / h[i]);
    //}

    double* A = new double[numberOfEq + 1];
    double* B = new double[numberOfEq];
    double* C = new double[numberOfEq];
    double* vec = new double[numberOfEq + 1];
    // cout << " Deb h " << h[1] << endl;
    A[0] = 1;
    C[0] = 0.5;
    vec[0] = (3 / h[0]) * ((table.y[1] - table.y[0]) / h[0] - table.A);
    for (int i = 1; i < numberOfEq; i++) {
        //cout << " Deb h " << h[1] << " " <<i << endl;
        B[i - 1] = h[i - 1];
        A[i] = 2 * (h[i - 1] + h[i]);
        C[i] = h[i];
        vec[i] = 6 * ((table.y[i + 1] - table.y[i]) / h[i] - (table.y[i] - table.y[i - 1]) / h[i - 1]);
    }
    B[numberOfEq - 1] = 0;
    A[numberOfEq] = 1;
    vec[numberOfEq] = table.B;
    //for (int i = 0; i < 2; i++) {
        //cout << " Deb matr " << matr[i][0] << " " << matr[i][1] << " " << matr[i][2] << " " << vec[i] <<endl;
    //}
    double* splainC = SolveSystem(A, B, C, vec, numberOfEq + 1);
    /* for (int i = 0; i < 3; i++) {
         cout << " deb C " << splainC[i] << endl;
     }*/
     //C
    for (int i = 0; i < numberOfEq; i++) {
        res[i].c[2] = splainC[i + 1];
    }
    //A
    for (int i = 0; i < numberOfEq; i++) {
        res[i].c[0] = table.y[i + 1];
    }
    //D
    for (int i = 0; i < numberOfEq; i++) {
        res[i].c[3] = (splainC[i + 1] - splainC[i]) / h[i];
    }
    //B
    for (int i = 0; i < numberOfEq; i++) {
        res[i].c[1] = h[i] * res[i].c[2] / 2 - h[i] * h[i] * res[i].c[3] / 6 + (table.y[i + 1] - table.y[i]) / h[i];
    }

    return res;
}

void HandInput() {
    bool isCorrectOrder;
    double A, B;
    double* xBuffer;
    int N, chose;
    cout << " Chose function " << endl;
    for (int i = 0; i < 9; i++) {
        cout << " " << i + 1 << " | " << TestFunc[i].fName << endl;
    }
    cout << " 0 | exit " << endl;
    cin >> chose;
    if (chose != 0) {
        cout << " Enter N A B ";
        cin >> N >> A >> B;
        if (N < 3) {
            cout << " IER 1 not enough points " << endl;
        }
        else {
            cout << " Enter X points " << endl;
            xBuffer = new double[N];
            for (int i = 0; i < N; i++) {
                cin >> xBuffer[i];
            }
            isCorrectOrder = true;
            for (int i = 1; i < N; i++) {
                if (xBuffer[i - 1] >= xBuffer[i]) {
                    isCorrectOrder = false;
                    break;
                }
            }
            if (!isCorrectOrder) {
                cout << " IER 2 wrong X order " << endl;
            }
            else {
                Table table(TestFunc[chose - 1], N, A, B, xBuffer);
                //table.Display();
                Splain* answer = SolveTable(table);
                for (int i = 0; i < N - 1; i++) {
                    cout << answer[i] << endl;
                }
                ofstream out("temp.dat");
                out << "f(x)= " << TestFunc[chose - 1].fName << "\n";
                for (int i = 0; i < N - 1; i++) {
                    out << "P" << i << "(x)= " << answer[i].ToString() << "\n";
                }
                out << "plot f(x) ls 1, ";
                out << " [" << ":" << table.x[1] << "] " << "P" << 0 << "(x) ls " << 2 << ", ";
                for (int i = 1; i < N - 2; i++) {
                    out << " [" << table.x[i] << ":" << table.x[i + 1] << "] " << "P" << i << "(x) ls " << i + 2 << ", ";
                }
                out << " [" << table.x[N - 2] << ":" << "] " << "P" << N - 2 << "(x) ls " << N << "\n";
                out << "\n" << "pause -1" << "\n";
                out.close();
                system("temp.dat");
            }
        }
    }
}

void InputFile(string FileName) {
    ifstream in;
    in.open(FileName);
    if (!in.is_open()) {
        cout << " Cant open file " << endl;
        return;
    }
    bool isCorrectOrder;
    double A, B;
    double* xBuffer;
    int N, funcId;
    in >> funcId;
    in >> N >> A >> B;
    if (N < 3) {
        cout << " IER 1 not enough points " << endl;
        return;
    }
    xBuffer = new double[N];
    for (int i = 0; i < N; i++) {
        in >> xBuffer[i];
    }
    in.close();
    isCorrectOrder = true;
    for (int i = 1; i < N; i++) {
        if (xBuffer[i - 1] >= xBuffer[i]) {
            isCorrectOrder = false;
            break;
        }
    }
    if (!isCorrectOrder) {
        cout << " IER 2 wrong X order " << endl;
        return;
    }
    Table table(TestFunc[funcId], N, A, B, xBuffer);
    table.Display();
    Splain* answer = SolveTable(table);
    for (int i = 0; i < N - 1; i++) {
        cout << answer[i] << endl;
    }
    ofstream out("temp.dat");
    out << "f(x)= " << TestFunc[funcId].fName << "\n";
    for (int i = 0; i < N - 1; i++) {
        out << "P" << i << "(x)= " << answer[i].ToString() << "\n";
    }
    out << "plot f(x) ls 1, ";
    out << " [" << ":" << table.x[1] << "] " << "P" << 0 << "(x) ls " << 2 << ", ";
    for (int i = 1; i < N - 2; i++) {
        out << " [" << table.x[i] << ":" << table.x[i + 1] << "] " << "P" << i << "(x) ls " << i + 2 << ", ";
    }
    out << " [" << table.x[N - 2] << ":" << "] " << "P" << N - 2 << "(x) ls " << N << "\n";
    out << "\n" << "pause -1" << "\n";
    out.close();
    system("temp.dat");
}

void InputFileWithY(string FileName) {
    ifstream in;
    in.open(FileName);
    if (!in.is_open()) {
        cout << " Cant open file " << endl;
        return;
    }
    bool isCorrectOrder;
    double A, B;
    double* xBuffer, * yBuffer;
    int N;
    in >> N >> A >> B;
    if (N < 3) {
        cout << " IER 1 not enough points " << endl;
        return;
    }
    xBuffer = new double[N];
    yBuffer = new double[N];
    for (int i = 0; i < N; i++) {
        in >> xBuffer[i];
    }
    for (int i = 0; i < N; i++) {
        in >> yBuffer[i];
    }
    in.close();
    isCorrectOrder = true;
    for (int i = 1; i < N; i++) {
        if (xBuffer[i - 1] >= xBuffer[i]) {
            isCorrectOrder = false;
            break;
        }
    }
    if (!isCorrectOrder) {
        cout << " IER 2 wrong X order " << endl;
        return;
    }
    Table table(N, A, B, xBuffer, yBuffer);
    table.Display();
    Splain* answer = SolveTable(table);
    for (int i = 0; i < N - 1; i++) {
        cout << answer[i] << endl;
    }
    ofstream out("temp.dat");
    out << "f(x)= 0" << "\n";
    for (int i = 0; i < N - 1; i++) {
        out << "P" << i << "(x)= " << answer[i].ToString() << "\n";
    }
    out << "plot f(x) ls 1, ";
    out << " [" << ":" << table.x[1] << "] " << "P" << 0 << "(x) ls " << 2 << ", ";
    for (int i = 1; i < N - 2; i++) {
        out << " [" << table.x[i] << ":" << table.x[i + 1] << "] " << "P" << i << "(x) ls " << i + 2 << ", ";
    }
    out << " [" << table.x[N - 2] << ":" << "] " << "P" << N - 2 << "(x) ls " << N << "\n";
    out << "\n" << "pause -1" << "\n";
    out.close();
    system("temp.dat");
}

Function TestFunc[] = {
        { [](double x) { return pow(x,3); },"x**3" },
        { [](double x) { return pow(x,2); },"x**2" },
        { [](double x) { return x; },"x" },
        { [](double x) { return (double)1; },"1" },
        { [](double x) { return 4 * pow(x,4) + 3 * pow(x,3) + 2 * pow(x,2) + x + 1; },"4*x**4 + 3*x**3 + 2*x**2 + x + 1"},
        { [](double x) { return sin(x); },"sin(x)" },
        { [](double x) { return 1 / (1 + 25 * x * x); }, "1/(1 + 25*x**2)"}
        //{ [](double x) { return log(x); },"log(x)" },
        //{ [](double x) { return abs(x); },"abs(x)" }
};

int main()
{
    //y vec
    int chose;

    do {
        cout << " Chose test option " << endl;
        cout << " 0 - keyboard input, 1-7 - test files, 8 - testY, 9 - exit " << endl;
        cin >> chose;
        switch (chose) {
        case 0: HandInput(); break;
        case 1: InputFile("test1.txt"); break;
        case 2: InputFile("test2.txt"); break;
        case 3: InputFile("test3.txt"); break;
        case 4: InputFile("test4.txt"); break;
        case 5: InputFile("test5.txt"); break;
        case 6: InputFile("test6.txt"); break;
        case 7: InputFile("test7.txt"); break;
        case 8: InputFileWithY("testY.txt"); break;
        default: cout << " Wrong option " << endl; break;
        }

    } while (chose != 9);

    return 0;
}