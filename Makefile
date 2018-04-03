all:
	cabal build
	ln -s dist/build/simulator/simulator
	ln -s dist/build/generator/generator
	ln -s dist/build/optimizer/optimizer
	ln -s dist/build/compiler/compiler
	cp dist/build/assembly/assembly assembly
	./generator
	./optimizer cpu.net
	./assembly clock.s
	./assembly fast_clock.s
	./assembly incr.s
	./compiler cpu_opt.net clock
	mv cpu_opt.c main1.c
	gcc main1.c -o clock_exec -w -Ofast -pthread -lpthread
	./compiler cpu_opt.net fast_clock
	mv cpu_opt.c main2.c
	gcc main2.c -o fast_clock_exec -w -Ofast -pthread -lpthread
	./compiler cpu_opt.net incr
	mv cpu_opt.c main3.c
	gcc main3.c -o incr_exec -w -Ofast -pthread -lpthread

clean:
	cabal clean
	rm simulator generator optimizer compiler assembly
	rm cpu.net cpu_opt.net
	rm main1.c main2.c main3.c
	rm clock fast_clock
	rm clock_exec fast_clock_exec
