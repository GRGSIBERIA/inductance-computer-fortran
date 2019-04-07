    module ReportFileClass
    implicit none
    
    type ReportFile
        character*32 partName
        double precision, dimension(:,:,:), allocatable :: positions    ! XYZ,時間,節点番号 => 節点番号はreport%maximumNodeIdに依存
    end type
    
    contains
    
    
    ! 個別のレポートファイルのデータ
    type(ReportFile) function init_ReportFile(fd, part, input, com) result(this)
        USE FileUtil
        USE InputFileClass
        USE CommonReportClass
    
        implicit none
        integer, intent(in) :: fd
        character*32, intent(in) :: part
        character*128, dimension(:), allocatable :: lines
        type(InputFile), intent(in) :: input
        type(CommonReport), intent(in) :: com
        
        CALL GetLines(fd, lines)
        
        this%partName = part
        
        
        
    end function
    
    end module