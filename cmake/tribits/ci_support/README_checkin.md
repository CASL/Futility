# Checkin Script with auto Merge Request
These steps are described in detail below:

 1. Prerequisites BEFORE starting script

  source /projects/vera/gcc-5.4.0/load_dev_env.sh  
  export GitLabUserId=<your Gitlab user id>  
  export PRIVATE_TOKEN=<Your gitlab account token>   
     - NOTE: It is strongly recommended that users do not put these values in your .bash_profile.
   All branches that have changes must have an upstream tracking branch.  

 2. Set up a CHECKIN directory  
  cd $VERA_BASE   
  mkdir BUILDS   
  cd BUILDS    
  mkdir CHECKIN  
  cd CHECKIN     
  ln -s ../../VERA/cmake/ctest/drivers/fissile4/checkin-test-vera.sh .    


 3. Test the builds of VERA locally with checkin-test script.  
  For example, to test all the packages locally without updating, run:   
  cd $VERA_BASE/BUILDS/CHECKIN    
  ./checkin-test-vera.sh --enable-all-packages=on \    
     --local-do-all    

Using `--enable-all-packages=on` will only enable VERA's primary meta-project packages and tests, not all defined packages from Trilinos, SCALE, etc.   
Instead of using `--enable-all-packages=on`, you can select individual packages with `--enable-packages=MPACT_exe`, for example.    
See the output from `checkin-test-vera.sh` for the valid package names.  The list of valid packages are determined on what repos you have cloned (and if they are 'Continuous' or 'Nightly' repos).   
By default this does just one build `--default-builds=MPI_RELEASE_DEBUG_SHARED`.   
If you also want to do a shared serial release build, for example, you can pass in `--st-extra-builds=SERIAL_RELEASE_SHARED` as well.    
Static builds are also supported with the names `MPI_RELEASE_DEBUG_STATIC`, `MPI_RELEASE_STATIC`, `SERIAL_RELEASE_STATIC`, etc.   
 See the source for the `checkin-test-vera.sh` script for a listing of all the defined builds and other information.   
 Using `do-configure.MPI_DEBUG_DEBUG_SHARED_GCC5.4.0` is a full debug build to enable using a debugger with `CMAKE_BUILD_TYPE=DEBUG` compiler options (i.e. `-O0 -g`) and full runtime debug checking `VERA_ENABLE_DEBUG=ON`.  '''WARNING:''' This is a very slow build of the code!   
You can create (or use precreated) other build configurations with custom do-configure scripts for whatever variations you would like.   
For a faster build using `CMAKE_BUILD_TYPE=RELEASE` compiler options (i.e. `-O3`) but with runtime debug checking `VERA_ENABLE_DEBUG=ON` (including `-fbounds-check` for Fortran), use the `do-configure` script:   
```
$VERA_FISSILE4_DIR/do-configure.MPI_RELEASE_DEBUG_SHARED_GCC540
```   
(In fact, if you don't need to use a debugger, this should be your default development build.)   
For a fully optimized build with no runtime checking, use:

VERA_DIR/cmake/ctest/driver/fissile4/do-configure.MPI_RELEASE_SHARED_GCC540    
If you want to modify a standard `do-configure` script, please copy it instead of creating a symbolic link then modify it.  You can override any settings by passing in cache variables `-D<VAR_NAME>=<VALUE>`, you should *not* modify the `*-options.cmake` files that are referred to in the script!

