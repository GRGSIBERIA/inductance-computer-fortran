    module CoilClass
    implicit none
    
    type Coil
        double precision, dimension(:,:), allocatable :: positions, forwards, rights  ! 次元, 時間
        double precision :: height, radius
    end type
    
    contains
    
    function ComputeCentroid(report) result(centroid)
        USE ReportFileClass
        implicit none
        type(ReportFile), intent(in) :: report
        double precision, dimension(:,:), allocatable :: centroid
        integer ti, ni
        double precision diff
        
        diff = 1.0d0 / report%numofEnableNodes
        
        ALLOCATE (centroid(3, report%numofTimes))
        
        ! 重心を求める
        do ni = 1, report%numofNodes
            if (report%enableNodeIds(ni) == 1) then
                do ti = 1, report%numofTimes
                    centroid(:,ti) = centroid(:,ti) + report%positions(:,ti,ni)
                end do
            end if
        end do
        
        ! 重心の平均を取る
        centroid = centroid * diff
        
    end function
    
    function ComputeForward(numofTimes, top, bottom) result(forward)
        implicit none
        integer, intent(in) :: numofTimes
        double precision, dimension(3,numofTimes), intent(in) :: top, bottom
        double precision, dimension(:,:), allocatable :: forward
        integer ti
        
        ALLOCATE (forward(3,numofTimes))
        
        ! 適当な前向きベクトルを取得する
        forward = top - bottom
        do ti = 1, numofTimes
            forward(:,ti) = forward(:,ti) / SQRT(DOT_PRODUCT(forward(:,ti), forward(:,ti)))
        end do
    end function
    
    function ComputeRight(numofTimes, forwards) result(rights)
        USE Math
        implicit none
        integer, intent(in) :: numofTimes
        double precision, dimension(3,numofTimes), intent(in) :: forwards
        double precision, dimension(:,:), allocatable :: rights
        integer ti
        
        ALLOCATE (rights(3,numofTimes))
        
        ! 入れ替えの方法で直角なベクトルを作成する
        do ti = 1, numofTimes
            rights(1,numofTimes) = forwards(2,numofTimes)
            rights(2,numofTimes) = -forwards(1,numofTimes)
            rights(3,numofTimes) = forwards(3,numofTimes)
        end do
        
        ! 直角なベクトルをベースにして外積で右手のベクトルを作成する
        do ti = 1, numofTimes
            rights(:,numofTimes) = Cross(forwards(:,numofTimes), rights(:,numofTimes)
        end do
        
    end function
    
    end function
    
    type(Coil) function init_Coil(input, topReport, bottomReport, com) result(this)
        USE ReportFileClass
        USE InputFileClass
        USE CommonReportClass
        implicit none
        type(InputFile), intent(in) :: input
        type(ReportFile), intent(in) :: topReport, bottomReport
        type(CommonReport), intent(in) :: com
        
        double precision, dimension(:,:), allocatable :: topCentroid, bottomCentroid
        
        ALLOCATE (this%positions(3,com%numofTimes))
        ALLOCATE (this%forwards(3,com%numofTimes))
        ALLOCATE (this%rights(3,com%numofTimes))
        
        ! 各レポートの重心を求めてコイルの中心座標を決める
        topCentroid = ComputeCentroid(topReport)
        bottomCentroid = ComputeCentroid(bottomReport)
        this%positions = (topCentroid - bottomCentroid) * 0.5d0 + bottomCentroid
        this%forwards = ComputeForward(com%numofTimes, topCentroid, bottomCentroid)
        
        DEALLOCATE (topCentroid)
        DEALLOCATE (bottomCentroid)
    end function
    
    end module