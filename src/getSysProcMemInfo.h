#pragma
extern "C"
{
  void getSysMemInfo(long long *totalPhysMem, long long *totalSwap, long long *totalFreeMem);
  void getProcMemInfo(long long *curUsage, long long *peakUsage);
}
