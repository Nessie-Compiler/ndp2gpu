#include <stdbool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifdef __linux
#include <CL/cl.h>
#else
#include <OpenCL/opencl.h>
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////

#define DEBUG_INFO      (0)
int		GROUP_SIZE      = 256;
#define NUM_BANKS       (16)
#define MAX_ERROR       (1e-7)
#define SEPARATOR       ("----------------------------------------------------------------------\n")

////////////////////////////////////////////////////////////////////////////////////////////////////

cl_device_id            ComputeDeviceId;
cl_command_queue        ComputeCommands;
cl_context              ComputeContext;
cl_program              ComputeProgram;
cl_kernel*              ComputeKernels;
cl_mem*                 ScanPartialSums = 0;
unsigned int            ElementsAllocated = 0;
unsigned int            LevelsAllocated = 0;

////////////////////////////////////////////////////////////////////////////////////////////////////

static char *
LoadProgramSourceFromFile(const char *filename)
{
    struct stat statbuf;
    FILE        *fh;
    char        *source;

    fh = fopen(filename, "r");
    if (fh == 0)
        return 0;

    stat(filename, &statbuf);
    source = (char *) malloc(statbuf.st_size + 1);
    fread(source, statbuf.st_size, 1, fh);
    source[statbuf.st_size] = '\0';

    return source;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv)
{
    int              err = 0;

    if (argc != 2) {
        printf ("Please provide one argument: the full path to the OpenCL file to check.\n");
        return -1;
    }
    
    // Connect to a GPU compute device
    //
    err = clGetDeviceIDs(NULL, CL_DEVICE_TYPE_ALL, 1, &ComputeDeviceId, NULL);
    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to locate a compute device!\n");
        return EXIT_FAILURE;
    }

    size_t returned_size = 0;
    cl_char vendor_name[1024] = {0};
    cl_char device_name[1024] = {0};
    err = clGetDeviceInfo(ComputeDeviceId, CL_DEVICE_VENDOR, sizeof(vendor_name), vendor_name, &returned_size);
    err|= clGetDeviceInfo(ComputeDeviceId, CL_DEVICE_NAME, sizeof(device_name), device_name, &returned_size);
    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to retrieve device info!\n");
        return EXIT_FAILURE;
    }

    printf(SEPARATOR);
    printf("Connecting to %s %s...\n", vendor_name, device_name);

    // Load the compute program from disk into a cstring buffer
    //
    printf(SEPARATOR);
    printf("Loading program '%s'...\n", argv[1]);
    printf(SEPARATOR);

    char *source = LoadProgramSourceFromFile(argv[1]);
    if(!source)
    {
        printf("Error: Failed to load compute program from file!\n");
        return EXIT_FAILURE;    
    }
    
    // Create a compute ComputeContext 
    //
    ComputeContext = clCreateContext(0, 1, &ComputeDeviceId, NULL, NULL, &err);
    if (!ComputeContext)
    {
        printf("Error: Failed to create a compute ComputeContext!\n");
        return EXIT_FAILURE;
    }

    // Create a command queue
    //
    ComputeCommands = clCreateCommandQueue(ComputeContext, ComputeDeviceId, 0, &err);
    if (!ComputeCommands)
    {
        printf("Error: Failed to create a command ComputeCommands!\n");
        return EXIT_FAILURE;
    }

    // Create the compute program from the source buffer
    //
    ComputeProgram = clCreateProgramWithSource(ComputeContext, 1, (const char **) & source, NULL, &err);
    if (!ComputeProgram || err != CL_SUCCESS)
    {
        printf("%s\n", source);
        printf("Error: Failed to create compute program!\n");
        return EXIT_FAILURE;
    }
    
    // Build the program executable
    //
    err = clBuildProgram(ComputeProgram, 0, NULL, NULL, NULL, NULL);
    if (err != CL_SUCCESS)
    {
        size_t length;
        char build_log[2048*2048];
        printf("%s\n", source);
        printf("Error: Failed to build program executable!\n");
        clGetProgramBuildInfo(ComputeProgram, ComputeDeviceId, CL_PROGRAM_BUILD_LOG, sizeof(build_log), build_log, &length);
        printf("%s\n", build_log);
        return EXIT_FAILURE;
    }

    printf("Compilation successful!\n");

    free(source);
    clReleaseProgram(ComputeProgram);
    clReleaseCommandQueue(ComputeCommands);
    clReleaseContext(ComputeContext);
    
    return 0;
}

