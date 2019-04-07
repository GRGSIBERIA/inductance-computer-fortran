    module ReportFileClass
    implicit none
    
    type ReportFile
        character*32 partName
        double precision, dimension(:,:,:), allocatable :: positions    ! XYZ,時間,節点番号 => 節点番号はreport%maximumNodeIdに依存
    end type
    
    contains
    
    ! ヘッダを探索してその文字列を返す手続き
    subroutine ScanHeader(lines, num, header)
        character*128, dimension(:), intent(in) :: lines
        integer, intent(inout) :: num
        character*128, intent(out) :: header
        integer xpos, i
        
        header = ""
        
        ! Xが存在する場所を探す
        do
            num = num + 1
            xpos = INDEX(lines(num), "  X  ")
            if (xpos > 0) then
                goto 100
            end if
        end do
100     continue
        
        header = TRIM(ADJUSTL(lines(num)(xpos+5:)))
        
        ! 戻る方向に探索しつつ，文字列を結合する
        i = 1
        do
            if (LEN_TRIM(ADJUSTL(lines(num-i))) < 1) then
                goto 200
            end if
            header = TRIM(ADJUSTL(lines(num-i))) // header
            i = i + 1
        end do
200     continue
    end subroutine
    
    
    
    ! 個別のレポートファイルのデータ
    type(ReportFile) function init_ReportFile(fd, input, com) result(this)
        USE FileUtil
        USE InputFileClass
        USE CommonReportClass
    
        implicit none
        integer, intent(in) :: fd
        character*128, dimension(:), allocatable :: lines
        type(InputFile), intent(in) :: input
        type(CommonReport), intent(in) :: com
        
        character*128 header    ! テスト
        integer num
        num = 1
        
        CALL GetLines(fd, lines)
        CALL ScanHeader(lines, num, header)
        
        DEALLOCATE (lines)
        
    end function
    
    end module