ben_thing: store.o store_c.o store_interface.o main.o
	gfortran -g store.o store_c.o main.o store_interface.o -lstdc++ -o ben_thing

store_interface.o: store_interface.f90
	gfortran -c store_interface.f90

store.o: store.cpp store.hpp
	g++ -g --std=c++11 -c store.cpp

store_c.o: store.o
	g++ -g --std=c++11 -c store_c.cpp

main.o: main.f90 store_interface.o
	gfortran -g -c main.f90

clean:
	rm *.o *.mod ben_thing
