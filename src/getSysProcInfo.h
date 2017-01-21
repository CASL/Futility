#pragma
extern "C"
{
  void getSysMemInfo(long long *totalPhysMem, long long *totalSwap, long long *totalFreeMem);
  void getProcMemInfo(long long *curUsage, long long *peakUsage);
  void getPWD_c(char* pwdpath, int* pathlen, int* status);
}
