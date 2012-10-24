#include <stdio.h>
#include <stdint.h>
int main() {
    uint64_t stop = 1;
    printf("printf Bug stop:%llu stop:%llu stop:%llu \n",
           stop,stop,stop);
    uint64_t ticks = 6796409;
    printf("printf Bug: ticks: %llu, ticks: %llu\n"
           "ticks(2f): %f, ticks(2f): %f\n"
           "ticks(2f2u): %llu ticks(2f2u): %llu\n"
           "ticks(2f/10^6): %f, ticks(2f/10^6): %f\n",
           ticks,ticks,(double)ticks,(double)ticks,
           (uint64_t)(double)ticks,(uint64_t)(double)ticks,
           (double)ticks/1000000,(double)ticks/1000000);
    return 0;
}

