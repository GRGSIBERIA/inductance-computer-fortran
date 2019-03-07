    module Preprocess
    implicit none
    
    contains
    
    ! 重心を計算する
    ! コイル面の重心座標を計算するのに使う
    function Centroid(numofPositions, positions)
        implicit none
        integer, intent(in) :: numofPositions
        real, dimension(numofPositions, 3), intent(inout) :: positions
        integer i
        real, dimension(3) :: Centroid
        Centroid = 0
        
        DO i = 1, numofPositions
            Centroid = Centroid + positions(i,:)
        END DO
        
        Centroid = Centroid / numofPositions
        
    end function
    
    ! 2つのベクトルを正規直交基底化する
    ! FORWARDベクトルが中心になる
    ! UPベクトルはFORWARD, RIGHTの順番で求められる
    subroutine OrthonormalBasis(forward, right)
        use Math
        implicit none
        real, dimension(3), intent(inout) :: forward, right
        real, dimension(3) :: up
        
        forward = Normalize(forward)
        right = Normalize(right)
        
        up = Cross(forward, right)
        right = Cross(up, forward)
    end subroutine
    
    subroutine PreprocessForCoilVector(coil, coilVec)
        use XYDataClass
        implicit none
        type(XYData), intent(in) :: coil
        type(XYData), intent(in) :: coilVec
        
    end subroutine
    
    end module