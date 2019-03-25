    module Math
    implicit none
    
    contains
    
    ! 外積
    function Cross(A, B)
        implicit none
        double precision, dimension(3), intent(in) :: A, B
        double precision, dimension(3) :: Cross
        
        double precision, dimension(3) :: A1, A2, B1, B2
        A1 = (/ A(2), A(3), A(1) /)
        A2 = (/ A(3), A(1), A(2) /)
        B1 = (/ B(3), B(1), B(2) /)
        B2 = (/ B(2), B(3), B(1) /)
        
        Cross = A1 * B1 - A2 * B2
    end function
    
    ! クォータニオンの実数部を求める
    double precision function MulQuaternion_R(Ar, Ai, Br, Bi) result(retR)
        implicit none
        double precision, dimension(3), intent(in) :: Ai, Bi
        double precision, intent(in) :: Ar, Br
        
        retR = Ar * Br - DOT_PRODUCT(Ai, Bi)
    end function
    
    ! クォータニオンの虚数部を求める
    function MulQuaternion_I(Ar, Ai, Br, Bi) result(retI)
        implicit none
        double precision, dimension(3), intent(in) :: Ai, Bi
        double precision, dimension(3) :: retI
        double precision, intent(in) :: Ar, Br
        
        retI = Ar * Bi + Ai * Br + Cross(Ai, Bi)
    end function
    
    ! ベクトルの長さを返す
    function Length(V)
        implicit none
        double precision ,dimension(3), intent(in) :: V
        double precision Length
        Length = SQRT(DOT_PRODUCT(V, V))
    end function
    
    ! ベクトルの正規化
    function Normalize(V)
        implicit none
        double precision, dimension(3), intent(in) :: V
        double precision, dimension(3) :: Normalize
        
        Normalize = V / Length(V)
    end function
    
    end module