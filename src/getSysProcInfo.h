/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
/                          Futility Development Group                          !
/                             All rights reserved.                             !
/                                                                              !
/ Futility is a jointly-maintained, open-source project between the University !
/ of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
/ can be found in LICENSE.txt in the head directory of this repository.        !
/+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#pragma
extern "C"
{
  void getSysMemInfo(long long *totalPhysMem, long long *totalSwap, long long *totalFreeMem);
  void getProcMemInfo(long long *curUsage, long long *peakUsage);
  void getPWD_c(char* pwdpath, int* pathlen, int* status);
}
