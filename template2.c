
    int* _ram = malloc(sizeof(int) * _ram_size);

    pthread_t myThreadSimulator;
    pthread_t myThreadPrinter;

    pthread_create(&myThreadSimulator, NULL, threadSimulator, _ram);
    pthread_create(&myThreadPrinter, NULL, threadPrinter, _ram);

    pthread_join(myThreadSimulator, NULL);
    pthread_join(myThreadPrinter, NULL);


    return 0;
}

void* threadPrinter (int* _ram)
{
    while(1) {
        _ram[28] = (int) time(NULL);
        f(_ram[0], _ram[4], _ram[8], _ram[12], _ram[20], _ram[24]);
        usleep (20000);
    }
    pthread_exit(NULL);
}


void* threadSimulator (int* _ram)
{
	long int _n = 100000000;
