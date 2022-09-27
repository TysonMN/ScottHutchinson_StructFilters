module WinAPI
    open System.Runtime.InteropServices

    [<DllImport(@"Kernel32.dll", CallingConvention = CallingConvention.Winapi)>]
    extern System.IntPtr LockResource(System.IntPtr hResourceData)
