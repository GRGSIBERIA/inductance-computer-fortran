    module ReportFileClass
    implicit none
    
    type ReportFile
        character*32 partName
        integer numofTimes, numofNodes      ! numofNodes は report%maximumNodeIdに依存しているので注意
        double precision, dimension(:,:,:), allocatable :: positions    ! XYZ,時間,節点番号 => 節点番号はreport%maximumNodeIdに依存
    end type
    
    contains
    
    ! Xの位置にnumがある状態でヘッダを作成する
    subroutine CombineHeader(lines, num, xpos, header)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        integer, intent(in) :: num, xpos
        character*128, intent(out) :: header
        integer i
        
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
    
    
    ! 記録されている節点数を返す
    integer function TotalRecordingNodes(lines) result(total)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        integer i
        total = 0
        
        do i = 1, SIZE(lines)
            if (INDEX(lines(i), "  X  ") > 0) then
                total = total + 1
            end if
        end do
    end function
    
    
    ! ヘッダ文字列から節点番号だけを取り出す
    integer function ExtractNodeId(header) result(nodeid)
        implicit none
        character*128, intent(in) :: header
        integer xpos
        
        xpos = INDEX(header, "N: ")
        READ (header(xpos+3:), "(I)") nodeid
    end function
    
    
    ! ヘッダ文字列から座標軸IDだけを取り出す
    integer function ExtractAxisId(header) result(axisid)
        implicit none
        character*128, intent(in) :: header
        integer xpos
        
        xpos = INDEX(header, "U:U")
        READ (header(xpos+3:xpos+4), "(I)") axisid
    end function
    
    
    ! ヘッダのみをスキャンしつつ，節点番号，座標軸番号，記録されている位置を記録する
    subroutine ScanNodeIds(lines, nodeIds, axisIds, xlinePoses)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        integer, dimension(:), intent(out) :: nodeIds, axisIds, xlinePoses
        character*128 header
        integer i, id, num, xpos
        
        axisIds = 0
        nodeIds = 0
        num = 1
        
        do i = 1, SIZE(lines)
            xpos = INDEX(lines(i), "  X  ")
            if (xpos > 0) then
                CALL CombineHeader(lines, i, xpos, header)
                nodeIds(num) = ExtractNodeId(header)
                axisIds(num) = ExtractAxisId(header)
                xlinePoses(num) = i + 1     ! timeidで加算する手前，本来の位置より-1あたり
                num = num + 1
            end if
        end do
        
    end subroutine
    
    ! positionsに記録していく
    subroutine RecordingPositions(lines, nodeIds, axisIds, xlinePoses, positions, numofTimes, maximumNodeId)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        integer, dimension(:), intent(in) :: nodeIds, axisIds, xlinePoses
        double precision, dimension(:,:,:), intent(out) :: positions
        integer, intent(in) :: numofTimes, maximumNodeId
        integer i, timeid
        double precision time
        
        positions = 0
        
        do i = 1, SIZE(xlinePoses)
            do timeid = 1, numofTimes
                if (nodeIds(i) <= maximumNodeId) then
                    READ (lines(xlinePoses(i)+timeid), *) time, positions(axisIds(i), timeid, nodeIds(i))
                end if
            end do
        end do
        
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
        
        character*128, dimension(:), allocatable :: headers
        integer, dimension(:), allocatable :: nodeIds, axisIds, xlinePoses
        
        integer totalNodes
        integer num
        num = 1
        
        ! すべての行を取得しておく
        CALL GetLines(fd, lines)
        totalNodes = TotalRecordingNodes(lines)
        
        ALLOCATE (headers(totalNodes))
        ALLOCATE (nodeIds(totalNodes))
        ALLOCATE (axisIds(totalNodes))
        ALLOCATE (xlinePoses(totalNodes))
        
        ALLOCATE (this%positions(3,com%numofTimes,input%maximumNodeId))
        this%positions = 0
        this%numofTimes = com%numofTimes
        this%numofNodes = input%maximumNodeId
        
        CALL ScanNodeIds(lines, nodeIds, axisIds, xlinePoses)
        CALL RecordingPositions(lines, nodeIds, axisIds, xlinePoses, this%positions, com%numofTimes, input%maximumNodeId)
        
        DEALLOCATE (lines)
        DEALLOCATE (headers)
        DEALLOCATE (nodeIds)
        DEALLOCATE (axisIds)
        DEALLOCATE (xlinePoses)
        
    end function
    
    end module