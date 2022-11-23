--> Tried installing the 'ocl-icd-opencl-dev' package 

--> Tried installing the oibaf mesa drivers (google it) : 
    just adding/removing a ppa (and then sudo apt update)
    --> Not working (clinfo throws an error : could not find file /usr/lib/clc/gfx1012...) :
        this file does not exist, but a similar one (gfx906...) does

--> Tried to install the amdgpu drivers : 'amdgpu-install_22.20.50200-1_all.deb'
    --> dependency errors
    --> tried installing the apt package 'amdgpu' : 
        needed to downgrade gcc and g++ to 11.2.0
        still wrong compiler version : kernel was built with 110200 and I have 110202 apparently
    --> this seems to work : sudo amdgpu-install --usecase=opencl --no-dkms
        and then apt install ocl-icd-opencl-dev
        at least clinfo seems good after this
    --> after rebooting : clinfo shows that 0 devices are recognized
    --> tried adding 'radeon' to /etc/modules : 
        after reboot, running lsmod shows that the radeon module is loaded, but still 0 devices in clinfo
    --> also tried adding 'amdgpu' to /etc/modules
    --> tried adding myself to the video and render groups

Finally working !! (for now)
--> What worked : 
    1) follow the instructions on https://docs.amd.com/bundle/ROCm-Installation-Guide-v5.3.3/page/How_to_Install_ROCm.html#d23e1966
       (Installer Script Method) :
        sudo apt-get update
        wget https://repo.radeon.com/amdgpu-install/5.3.3/ubuntu/jammy/amdgpu-install_5.3.50303-1_all.deb
        sudo apt-get install ./amdgpu-install_5.3.50303-1_all.deb
    2) Install opencl and openclsdk through rocm : 
       sudo amdgpu-install --usecase=opencl,openclsdk
    3) Install the rocm dkms (big package) : sudo apt install rocm-dkms
    4) Reboot
--> Everything interesting is in /opt/rocm-5.3.3 :
    - /opt/rocm-5.3.3/opencl has the openCL library (.so file) and headers (.h files)
    - /opt/rocm-5.3.3/bin has the rocminfo and clinfo scripts
      DON'T use the apt clinfo (/usr/bin/clinfo) : use /opt/rocm-5.3.3/bin/clinfo instead 
--> To compile and run a futhark program using openCL : 
    1) To help futhark find the openCL library, run the commands : 
       export LIBRARY_PATH=/opt/rocm-5.3.3/opencl/lib
       export LD_LIBRARY_PATH=/opt/rocm-5.3.3/opencl/lib 
    2) Compile the program : futhark opencl test.fut
    3) Run the program : ./test 