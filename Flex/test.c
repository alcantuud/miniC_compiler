#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    long long l = atoll(1 << 63);
    printf("%lld\n", l);
}
