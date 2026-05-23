#include <iostream>
#include <vector>
#include <string>

int main() {
    // 1. Salida básica por consola
    std::cout << "=== Ejemplo de C++ ===" << std::endl;

    // 2. Variables para la entrada del usuario
    std::string userName;
    int userAge;

    std::cout << "Introduce tu nombre: ";
    std::getline(std::cin, userName);

    std::cout << "Introduce tu edad: ";
    std::cin >> userAge;

    // 3. Listas (Vectores) de datos separados
    std::vector<std::string> names;
    std::vector<int> ages;

    // Agregar datos del usuario
    names.push_back(userName);
    ages.push_back(userAge);

    // Agregar datos predefinidos
    names.push_back("Alice");
    ages.push_back(30);

    // 4. Bucle tradicional para mostrar la información
    std::cout << "\n--- Presentando a todos ---" << std::endl;
    for (size_t i = 0; i < names.size(); ++i) {
        std::cout << "Hola, mi nombre es " << names[i] 
                  << " y tengo " << ages[i] << " años." << std::endl;
    }

    return 0; // Indica que el programa terminó con éxito
}
