#pragma
#ifdef WIN32
//This is for Windows systems
#include <Windows.h>
#include <WinBase.h>
#include <Psapi.h>

void getSysMemInfo(long long *totalPhysMem, long long *totalSwap, long long *totalFreeMem)
{
  MEMORYSTATUSEX memInfo;
  /*
  typedef struct _MEMORYSTATUSEX {
    // The size of the structure, in bytes. You must set this member before 
    // calling GlobalMemoryStatusEx.
    DWORD     dwLength;

    // A number between 0 and 100 that specifies the approximate percentage
    // of physical memory that is in use (0 indicates no memory use and 100
    // indicates full memory use).
    DWORD     dwMemoryLoad;

    // The amount of actual physical memory, in bytes.
    DWORDLONG ullTotalPhys;
    
    // The amount of physical memory currently available, in bytes. This is 
    // the amount of physical memory that can be immediately reused without
    // having to write its contents to disk first. It is the sum of the size
    // of the standby, free, and zero lists.
    DWORDLONG ullAvailPhys;
    
    // The current committed memory limit for the system or the current process,
    // whichever is smaller, in bytes. To get the system-wide committed memory 
    // limit, call GetPerformanceInfo.
    DWORDLONG ullTotalPageFile;
    
    // The maximum amount of memory the current process can commit, in bytes.
    // This value is equal to or smaller than the system-wide available commit
    // value. To calculate the system-wide available commit value, call
    // GetPerformanceInfo and subtract the value of CommitTotal from the value
    // of CommitLimit.
    DWORDLONG ullAvailPageFile;
    
    // The size of the user-mode portion of the virtual address space of the
    // calling process, in bytes. This value depends on the type of process,
    // the type of processor, and the configuration of the operating system.
    // For example, this value is approximately 2 GB for most 32-bit processes
    // on an x86 processor and approximately 3 GB for 32-bit processes that are
    // large address aware running on a system with 4-gigabyte tuning enabled.
    DWORDLONG ullTotalVirtual;
    
    // The amount of unreserved and uncommitted memory currently in the
    // user-modeportion of the virtual address space of the calling process,
    // in bytes.
    DWORDLONG ullAvailVirtual;
    
    //Reserved. This value is always 0.
    DWORDLONG ullAvailExtendedVirtual;
  } MEMORYSTATUSEX, *LPMEMORYSTATUSEX;
  */

  memInfo.dwLength = sizeof(MEMORYSTATUSEX);
  GlobalMemoryStatusEx(&memInfo);

  *totalPhysMem  = memInfo.ullTotalPhys;
  *totalSwap     = memInfo.ullTotalPageFile - memInfo.ullTotalPhys;
  *totalFreeMem  = memInfo.ullAvailPhys;
}

void getProcMemInfo(long long *curUsage, long long *peakUsage)
{
  PROCESS_MEMORY_COUNTERS   pmc;

  GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc));
  *curUsage  = pmc.PagefileUsage;
  *peakUsage = pmc.PeakPagefileUsage;
}

#else
//This is for Linux/Unix systems
#include <sys/sysinfo.h>
#include <sys/resource.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void getSysMemInfo(long long *totalPhysMem, long long *totalSwap, long long *totalFreeMem)
{
  struct sysinfo memInfo;
  
  sysinfo (&memInfo);

  *totalPhysMem  = memInfo.totalram;
  *totalPhysMem *= memInfo.mem_unit;
  *totalSwap     = memInfo.totalswap;
  *totalSwap    *= memInfo.mem_unit;
  *totalFreeMem  = memInfo.freeram;
  *totalFreeMem *= memInfo.mem_unit;
}

void getProcMemInfo(long long *curUsage, long long *peakUsage)
{
  struct rusage usage;
  FILE * file;
  char line[128];

  getrusage(RUSAGE_SELF, &usage);
  *peakUsage = usage.ru_maxrss;

  file = fopen("/proc/self/status", "r");
  while (fgets(line, 128, file) != NULL)
  {
    if (strncmp(line, "VmSize:", 7) == 0)
    {
      *curUsage = parseLine(line);
      break;
    }
  }
  fclose(file);
  
  *peakusage *= 1024;
}

int parseLine(char * line)
{
  int i = strlen(line);
  while (*line < '0' || *line > '9') line++;
  line[i-3] = '\0';
  i = atoi(line);
  return i;
}
#endif
