
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
        int t0 = (int) time(NULL);
        _ram[28] = t0 & 255;
        _ram[29] = (t0 >> 8) & 255;
        _ram[30] = (t0 >> 16) & 255;
        _ram[31] = (t0 >> 24) & 255;
        f(_ram[0], _ram[4], _ram[8], _ram[12], _ram[20], _ram[24]);
        usleep (20000);
    }
    pthread_exit(NULL);
}


void* threadSimulator (int* _ram)
{
	long int _n = -1;
