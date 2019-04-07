    module CommonReportClass
    implicit none
    
    type CommonReport
        integer numofTimes
        double precision, dimension(:), allocatable :: times
    end type
        
    contains
    
    ! 時間の数を取得する関数
    integer function GetNumberOfTimes(fd) result(numofTimes)
        USE FileUtil
        
        implicit none
        integer, intent(in) :: fd
        character*128 line
        
        numofTimes = 0
        REWIND (fd)
        CALL ScanFirstRecord(fd)
        
        do
            READ (fd, "(A)") line
            if (LEN_TRIM(line) <= 2) then   ! 2文字以下はさすがにないやろ
                goto 200
            end if
            numofTimes = numofTimes + 1
        end do
200     continue        
    end function
    
    
    ! 時間を取得する関数
    subroutine GetTimes(fd, com)
        USE FileUtil
        
        implicit none
        integer, intent(in) :: fd
        type(CommonReport), intent(out) :: com
        double precision val
        integer i
        
        com%numofTimes = GetNumberOfTimes(fd)   ! 先に時間の数を取得する
        
        ALLOCATE (com%times(com%numofTimes))    ! メモリを確保
        
        REWIND (fd)
        CALL ScanFirstRecord(fd)
        
        do i = 1, com%numofTimes
            READ (fd, *) com%times(i), val      ! データを代入していく
        end do
        
    end subroutine
    
    ! 共通で使うデータ
    type(CommonReport) function init_CommonReport(fd) result(this)
        USE InputFileClass
        implicit none
        integer, intent(in) :: fd
        
        CALL GetTimes(fd, this) ! numofTimes, timesの2つをここで初期化する
        
    end function
    
    end module