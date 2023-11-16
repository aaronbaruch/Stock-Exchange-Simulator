Caml1999M032����            +src/user.ml����  y�  �  T�  S�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����&stocks��.<command-line>A@A�A@G@@��A@@�A@H@@@@�@@�������@�@@@�@@�@@@@�@@@�@����������$Data��+src/user.mlA@E�A@I@��A@E�A@I@@A��A@@�A@I@@��
A@@�A@I@��������#Lwt��CKP�CKS@��CKP�CKS@@A��CKK�CKS@���)ocaml.doc���@@ ���@@ �A�������	F Module defines User and functions to interact with a financial system��.DTT�/DT _@@��1DTT�2DT _@@@@��4DTT�5DT _@@��7DTT�8DT _@@��:CKK�;CKS@�����$User��CF a m�DF a q@������A�  # �!t��PG x �QG x �@@@@A@���4��@@ ��@@ �A�������7 Type representing user��aH � ��bH � �@@��dH � ��eH � �@@@@��gH � ��hH � �@@��jH � ��kH � �@@��mG x z�nG x �@@��pG x z�qG x �@���Р)init_user��yJ � ��zJ � �@��@����&string���J � ���J � �@@���J � ���J � �@@@��@����#int���J � ���J � �@@���J � ���J � �@@@����!t���J � ���J � �@@���J � ���J � �@@@���J � ���J � �@@@���J � ���J � �@@@@������Z@@ ��[@@ �A�������1 Initialize User ���K � ���K � �@@���K � ���K � �@@@@���K � ���K � �@@���K � ���K � �@@���J � ���J � �@���J � ���J � �@���Р'deposit���M � ���M � �@��@����!t���M � ���M � �@@���M � ���M � �@@@��@����#int���M � ���M � �@@���M � ���M � �@@@����!t���M � ���M � �@@���M � ���M � �@@@���M � ���M � �@@@���M � ���M � �@@@@���ܰ��@@ ���@@ �A�������	# Deposit money into user's account ��	N � �
N �(@@��N � �N �(@@@@��N � �N �(@@��N � �N �(@@��M � ��M � �@��M � ��M � �@���Р(withdraw��!P*0�"P*8@��@����!t��+P*;�,P*<@@��.P*;�/P*<@@@��@����#int��8P*@�9P*C@@��;P*@�<P*C@@@����!t��CP*G�DP*H@@��FP*G�GP*H@@@��IP*@�JP*H@@@��LP*;�MP*H@@@@���0��@@ ��@@ �A�������	$ Withdraw money from user's account ��]QIK�^QIt@@��`QIK�aQIt@@@@��cQIK�dQIt@@��fQIK�gQIt@@��iP*,�jP*H@��lP*,�mP*H@���Р'balance��uSv|�vSv�@��@����!t��Sv���Sv�@@���Sv���Sv�@@@����#int���Sv���Sv�@@���Sv���Sv�@@@���Sv���Sv�@@@@���t��F@@ ��G@@ �A�������	  Check balance of user's account���T����T��@@���T����T��@@@@���T����T��@@���T����T��@@���Svx��Sv�@���Svx��Sv�@���Р)portfolio���V����V��@��@����!t���V����V��@@���V����V��@@@����$list���V����V��@��������&string���V����V��@@���V����V��@@@�����#int���V����V��@@���V����V��@@@@���V����V��@@@@���V����V��@@@���V����V��@@@@���ְ��@@ ���@@ �A�������	" Check Portfolio of user's account��W���W�@@��W���W�@@@@��	W���
W�@@��W���W�@@��V���V��@��V���V��@���Р$sell��Y�Y@��@����!t��%Y�&Y@@��(Y�)Y@@@��@����&string��2Y �3Y&@@��5Y �6Y&@@@��@����#int��?Y*�@Y-@@��BY*�CY-@@@����!t��JY1�KY2@@��MY1�NY2@@@��PY*�QY2@@@��SY �TY2@@@��VY�WY2@@@@���:��@@ ��@@ �A�������	# Sell stocks from User's portfolio ��gZ35�hZ3]@@��jZ35�kZ3]@@@@��mZ35�nZ3]@@��pZ35�qZ3]@@��sY�tY2@��vY�wY2@���Р#buy��\_e��\_h@��@����!t���\_k��\_l@@���\_k��\_l@@@��@����&string���\_p��\_v@@���\_p��\_v@@@��@����#int���\_z��\_}@@���\_z��\_}@@@����!t���\_���\_�@@���\_���\_�@@@���\_z��\_�@@@���\_p��\_�@@@���\_k��\_�@@@@������p@@ ��q@@ �A�������	! Buy stocks for User's portfolio ���]����]��@@���]����]��@@@@���]����]��@@���]����]��@@���\_a��\_�@���\_a��\_�@���Р(next_day���_����_��@��@����!t���_����_��@@���_����_��@@@����!t���_����_��@@���_����_��@@@���_����_��@@@@������@@ ���@@ �A�������? Move the user to the next day ��`���`��@@��`���`��@@@@��`���`��@@��`���`��@@��_���_��@��_���_��@���Р0display_username��'b���(b�@��@����!t��1b��2b�@@��4b��5b�@@@����&string��<b��=b�@@��?b��@b�@@@��Bb��Cb�@@@@@��Eb���Fb�@��Hb���Ib�@@��KF a t�Lc@@@��NF a a�Oc@��QF a a�Rc@������(UserImpl��[f5<�\f5D@�������A�  # �!t��igU\�jgU]@@@��Р(username��qhbf�rhbn@@����&string��yhbq�zhbw@@��|hbq�}hbw@@@��hbf��hbx@@�Р'balance���iy}��iy�@@����#int���iy���iy�@@���iy���iy�@@@���iy}��iy�@@�Р&stocks���j����j��@@����$list���j����j��@��������&string���j����j��@@���j����j��@@@�����#int���j����j��@@���j����j��@@@@���j����j��@@@@���j����j��@@@���j����j��@@�Р#day���k����k��@@����#int���k����k��@@���k����k��@@@���k����k��@@@A@@���gUW��l��@@���gUW��l��@������'DataAPI���n����n��@����'DataAPI���n����n��@���n����n��@@@���n����n��@���n����n��@�����*ocaml.text���@@ ���@@ �A�������	H Type representation of User, represent's a user's critical information ��p���p�-@@��p���p�-@@@@��p���p�-@@��p���p�-@��p���p�-@���@�����)init_user��)u���*u��@��,u���-u��@@@��@@�����(username��7u���8u��@��:u���;u��@@@����&string��Bu���Cu�@@��Eu���Fu�@@@��Hu���Iu�@@@��@@�����'balance��Su��Tu�@��Vu��Wu�@@@����#int��^u��_u�@@��au��bu�@@@��du��eu�@@@�  ������(username��pv!�qv)A������wv!�xv)@��zv!�{v)@@@����'balance���v+��v2A�������v+��v2@���v+��v2@@@����&stocks���v4��v:@����"[]���v=��v?@@���v=��v?@@@����#day���vA��vD@���!0@���vG��vH@@@@@���v��vJ@@@����!t���u���u�@@���u���u�@@@���u���vJ@@@���u���vJA@@���u����vJA@@������{@@ ��|@@ �A�������	� [init_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. ���r/1��t��@@���r/1��t��@@@@���r/1��t��@@���r/1��t��@@���u����vJ@@���u����vJ@���@�����'deposit���y����y��@���y����y��@@@��@@�����$user���y��� y��@��y���y��@@@����!t��
y���y��@@��y���y��@@@��y���y��@@@��@@�����!n��y���y��@��y���y��@@@����#int��&y���'y��@@��)y���*y��@@@��,y���-y��@@@������'balance��6y���7y��@������!+��@y���Ay��@��Cy���Dy��@@@��@������$user��Oy���Py��@��Ry���Sy��@@@��'balance��Xy���Yy��@��[y���\y��@@@��@����!n��ey���fy��@��hy���iy��@@@@��ky���ly��@@@@�����$user��ty���uy��@��wy���xy��@@@��zy���{y��@@@��}y���~y��A@@���y����y��A@@���d��6@@ ��7@@ �A�������	> [deposit user n] increases the user's balance by [n] dollars ���xLN��xL�@@���xLN��xL�@@@@���xLN��xL�@@���xLN��xL�@@���y����y��@@���y����y��@���@�����(withdraw���|(.��|(6@���|(.��|(6@@@��@@�����$user���|(8��|(<@���|(8��|(<@@@����!t���|(?��|(@@@���|(?��|(@@@@���|(7��|(A@@@��@@�����!n���|(C��|(D@���|(C��|(D@@@����#int���|(G��|(J@@���|(G��|(J@@@���|(B��|(K@@@������'balance���|(Z��|(a@������!-���|(q��|(r@���|(q��|(r@@@��@������$user��
|(d�|(h@��|(d�|(h@@@��'balance��|(i�|(p@��|(d�|(p@@@��@����!n�� |(s�!|(t@��#|(s�$|(t@@@@��&|(d�'|(t@@@@�����$user��/|(P�0|(T@��2|(P�3|(T@@@��5|(N�6|(v@@@��8|(B�9|(vA@@��;|(7�<|(vA@@������@@ ���@@ �A�������	? [withdraw user n] decreases the user's balance by [n] dollars ��L{���M{�'@@��O{���P{�'@@@@��R{���S{�'@@��U{���V{�'@@��X|(*�Y|(v@@��[|(*�\|(v@���@�����'balance��g���h��@��j���k��@@@��@@�����$user��u���v��@��x���y��@@@����!t���������@@���������@@@���������@@@�  ������$user���������@���������@@@��'balance���������@���������@@@����#int���������@@���������@@@���������@@@���������A@@������e@@ ��f@@ �A�������	5 [balance user] returns an int of the user's balance ���~xz��~x�@@���~xz��~x�@@@@���~xz��~x�@@���~xz��~x�@@���������@@���������@���@�����)portfolio��� B28�� B2A@��� B28�� B2A@@@��@@�����$user��� B2C�� B2G@��� B2C�� B2G@@@����!t��� B2J�� B2K@@��� B2J�� B2K@@@��� B2B�� B2L@@@�  ������$user�� B2e� B2i@��	 B2e�
 B2i@@@��&stocks�� B2j� B2p@�� B2e� B2p@@@����$list�� B2^� B2b@��������&string��& B2P�' B2V@@��) B2P�* B2V@@@�����#int��2 B2Y�3 B2\@@��5 B2Y�6 B2\@@@@��8 B2P�9 B2\@@@@��; B2O�< B2b@@@��> B2M�? B2p@@@��A B2B�B B2pA@@���%���@@ ���@@ �A�������	F [balance user] returns an (string,int) list of the user's portfolio. ��R A���S A�1@@��U A���V A�1@@@@��X A���Y A�1@@��[ A���\ A�1@@��^ B24�_ B2p@@��a B24�b B2p@���@�����+able_to_buy��m Drx�n Dr�@��p Drx�q Dr�@@@��@@�����$user��{ Dr��| Dr�@��~ Dr�� Dr�@@@����!t��� Dr��� Dr�@@��� Dr��� Dr�@@@��� Dr��� Dr�@@@��@@�����%index��� Dr��� Dr�@��� Dr��� Dr�@@@����&string��� Dr��� Dr�@@��� Dr��� Dr�@@@��� Dr��� Dr�@@@��@@�����!n��� Dr��� Dr�@��� Dr��� Dr�@@@����#int��� Dr��� Dr�@@��� Dr��� Dr�@@@��� Dr��� Dr�@@@�  ��@�����,ticker_price��� E���� E��@��� E���� E��@@@�������(Lwt_main#run��� F���� F��@��� F���� F��@@@��@������#>>=��� G��� G�	@��� G��� G�	@@@��@�������'DataAPI0get_ticker_price��  G��� G��@�� G��� G��@@@��@����%index�� G� � G�@�� G� � G�@@@@�� G��� G�@@@��@��@@���0ticker_price_str�� G�� G�@��! G��" G�@@@�������#Lwt&return��- H",�. H"6@��0 H",�1 H"6@@@��@������,int_of_float��< H"8�= H"D@��? H"8�@ H"D@@@��@������/float_of_string��K H"F�L H"U@��N H"F�O H"U@@@��@����0ticker_price_str��X H"V�Y H"f@��[ H"V�\ H"f@@@@��^ H"E�_ H"g@���b H"F�c H"f@@@@��e H"7�f H"h@���i H"8�j H"g@@@@��l H",�m H"h@@@��o G�
�p H"h@@@@��r G���s H"j@���v G���w H"h@@@@��y F���z H"j@@@@��| E���} H"j@@������"<=��� L���� L��@��� L���� L��@@@��@������!*��� L���� L��@��� L���� L��@@@��@����,ticker_price��� L���� L��@��� L���� L��@@@��@����!n��� L���� L��@��� L���� L��@@@@��� L���� L��@@@��@������$user��� L���� L��@��� L���� L��@@@��'balance��� L���� L��@��� L���� L��@@@@��� L���� L��@@@��� E���� L��@@@����$bool��� Dr��� Dr�@@��� Dr��� Dr�@@@��� Dr��� L��@@@��� Dr��� L��A@@��� Dr��� L��A@@��� Dr��� L��A@@@��� Drt�� L��@@��� Drt�� L��@���A�����7update_user_stocks_list��� N�	�� N�	@��� N�	�	  N�	@@@��@@�����&stocks��	
 N�	 �	 N�	&@��	 N�	 �	 N�	&@@@����$list��	 N�	8�	 N�	<@��������&string��	! N�	*�	" N�	0@@��	$ N�	*�	% N�	0@@@�����#int��	- N�	3�	. N�	6@@��	0 N�	3�	1 N�	6@@@@��	3 N�	*�	4 N�	6@@@@��	6 N�	)�	7 N�	<@@@��	9 N�	�	: N�	=@@@��@@�����%index��	D O	>	E�	E O	>	J@��	G O	>	E�	H O	>	J@@@����&string��	O O	>	M�	P O	>	S@@��	R O	>	M�	S O	>	S@@@��	U O	>	D�	V O	>	T@@@��@@�����!n��	` O	>	V�	a O	>	W@��	c O	>	V�	d O	>	W@@@����#int��	k O	>	Z�	l O	>	]@@��	n O	>	Z�	o O	>	]@@@��	q O	>	U�	r O	>	^@@@�  ������&stocks��	} P	w	��	~ P	w	�@��	� P	w	��	� P	w	�@@@��������	� Q	�	��	� Q	�	�@@��	� Q	�	��	� Q	�	�@@@@����&stocks��	� Q	�	��	� Q	�	�@��	� Q	�	��	� Q	�	�@@@������"::��	� R	�	��	� R	�	�@��@���������!k��	� R	�	��	� R	�	�@��	� R	�	��	� R	�	�@@@����!m��	� R	�	��	� R	�	�@��	� R	�	��	� R	�	�@@@@��	� R	�	��	� R	�	�@���	� R	�	��	� R	�	�@@@����!t��	� R	�	��	� R	�	�@��	� R	�	��	� R	�	�@@@@��	� R	�	��	� R	�	�A@@��	� R	�	��	� R	�	�@@@@��������!=��	� S	�	��	� S	�	�@��	� S	�	��	� S	�	�@@@��@����!k��	� S	�	��	� S	�	�@��	� S	�	��	� S	�	�@@@��@����%index��	� S	�	��	� S	�	�@��	� S	�	��
  S	�	�@@@@��
 S	�	��
 S	�	�@@@����i��
	 S	�	��

 S	�	�@�����������!k��
 S	�	��
 S	�	�@��
 S	�	��
 S	�	�@@@���������
% S	�	��
& S	�	�@��
( S	�	��
) S	�	�@@@��@����!m��
2 S	�	��
3 S	�	�@��
5 S	�	��
6 S	�	�@@@��@����!n��
? S	�	��
@ S	�	�@��
B S	�	��
C S	�	�@@@@��
E S	�	��
F S	�	�@@@@��
H S	�	��
I S	�	�@���
L S	�	��
M S	�	�@@@�������7update_user_stocks_list��
W S	�	��
X S	�	�@��
Z S	�	��
[ S	�	�@@@��@����!t��
d S	�	��
e S	�	�@��
g S	�	��
h S	�	�@@@��@����%index��
q S	�	��
r S	�	�@��
t S	�	��
u S	�	�@@@��@����!n��
~ S	�	��
 S	�	�@��
� S	�	��
� S	�	�@@@@��
� S	�	��
� S	�	�@@@@��
� S	�	��
� S	�	�A@@��
� S	�	��
� S	�	�@@@�������
� T	�
�
� T	�
@�����������!k��
� T	�
�
� T	�
@��
� T	�
�
� T	�
@@@�����!m��
� T	�
�
� T	�
@��
� T	�
�
� T	�
@@@@��
� T	�
�
� T	�
@���
� T	�
�
� T	�
@@@�������7update_user_stocks_list��
� T	�
�
� T	�
-@��
� T	�
�
� T	�
-@@@��@����!t��
� T	�
.�
� T	�
/@��
� T	�
.�
� T	�
/@@@��@����%index��
� T	�
0�
� T	�
5@��
� T	�
0�
� T	�
5@@@��@����!n��
� T	�
6�
� T	�
7@��
� T	�
6�
� T	�
7@@@@��
� T	�
�
� T	�
7@@@@��
� T	�
�
� T	�
7A@@��
� T	�
�
� T	�
7@@@��
� S	�	��
� T	�
7@@@@��
� P	w	{�
� T	�
7@@@����$list�� O	>	p� O	>	t@��������&string�� O	>	b� O	>	h@@�� O	>	b� O	>	h@@@�����#int�� O	>	k� O	>	n@@�� O	>	k� O	>	n@@@@��! O	>	b�" O	>	n@@@@��$ O	>	a�% O	>	t@@@��' O	>	_�( T	�
7@@@��* O	>	U�+ T	�
7A@@��- O	>	D�. T	�
7A@@��0 N�	�1 T	�
7A@@@��3 N���4 T	�
7@@��6 N���7 T	�
7@���@�����5update_new_stock_list��B V
9
?�C V
9
T@��E V
9
?�F V
9
T@@@��@@�����&stocks��P V
9
V�Q V
9
\@��S V
9
V�T V
9
\@@@����$list��[ V
9
n�\ V
9
r@��������&string��g V
9
`�h V
9
f@@��j V
9
`�k V
9
f@@@�����#int��s V
9
i�t V
9
l@@��v V
9
i�w V
9
l@@@@��y V
9
`�z V
9
l@@@@��| V
9
_�} V
9
r@@@�� V
9
U�� V
9
s@@@��@@�����%index��� V
9
u�� V
9
z@��� V
9
u�� V
9
z@@@����&string��� V
9
}�� V
9
�@@��� V
9
}�� V
9
�@@@��� V
9
t�� V
9
�@@@��@@�����!n��� W
�
��� W
�
�@��� W
�
��� W
�
�@@@����#int��� W
�
��� W
�
�@@��� W
�
��� W
�
�@@@��� W
�
��� W
�
�@@@�  ��@�����+user_stocks��� X
�
��� X
�
�@��� X
�
��� X
�
�@@@������7update_user_stocks_list��� X
�
��� X
�
�@��� X
�
��� X
�
�@@@��@����&stocks��� X
�
��� X
�
�@��� X
�
��� X
�
�@@@��@����%index��� X
�
��� X
�
�@��� X
�
��� X
�
�@@@��@����!n��� X
�
��� X
�
�@��� X
�
��� X
�
�@@@@��� X
�
��� X
�
�@@@@�� X
�
�� X
�
�@@��������+�� Y
� � Y
�@�� Y
� � Y
�@@@��@����+user_stocks�� Y
�
�� Y
�
�@�� Y
�
�� Y
�
�@@@��@����&stocks��& Y
��' Y
�@��) Y
��* Y
�@@@@��, Y
�
��- Y
�@@@�������3 Y
��4 Y
�@�����������%index��B Y
��C Y
�@��E Y
��F Y
�@@@�����!n��N Y
��O Y
�@��Q Y
��R Y
�@@@@��T Y
��U Y
�@���X Y
��Y Y
�@@@�����&stocks��a Y
��b Y
�"@��d Y
��e Y
�"@@@@��g Y
��h Y
�"A@@��j Y
��k Y
�"@@@�����+user_stocks��s Y
�(�t Y
�3@��v Y
�(�w Y
�3@@@��y Y
�
��z Y
�3@@@��| X
�
��} Y
�3@@@����$list��� W
�
��� W
�
�@��������&string��� W
�
��� W
�
�@@��� W
�
��� W
�
�@@@�����#int��� W
�
��� W
�
�@@��� W
�
��� W
�
�@@@@��� W
�
��� W
�
�@@@@��� W
�
��� W
�
�@@@��� W
�
��� Y
�3@@@��� W
�
��� Y
�3A@@��� V
9
t�� Y
�3A@@��� V
9
U�� Y
�3A@@@��� V
9
;�� Y
�3@@��� V
9
;�� Y
�3@���@�����;subtract_and_update_balance��� [5;�� [5V@��� [5;�� [5V@@@��@@�����$user��� [5X�� [5\@��� [5X�� [5\@@@����!t��� [5_�� [5`@@��� [5_�� [5`@@@��� [5W�� [5a@@@��@@�����%index��� [5c�� [5h@��� [5c�� [5h@@@����&string��� [5k�� [5q@@��� [5k�� [5q@@@��� [5b�� [5r@@@��@@�����!n��	 [5t�
 [5u@�� [5t� [5u@@@����#int�� [5x� [5{@@�� [5x� [5{@@@�� [5s� [5|@@@�  ��@�����,ticker_price��' \���( \��@��* \���+ \��@@@�������(Lwt_main#run��6 ]���7 ]��@��9 ]���: ]��@@@��@������#>>=��E ^���F ^��@��H ^���I ^��@@@��@�������'DataAPI0get_ticker_price��V ^���W ^��@��Y ^���Z ^��@@@��@����%index��c ^���d ^��@��f ^���g ^��@@@@��i ^���j ^��@@@��@��@@���0ticker_price_str��t ^���u ^��@��w ^���x ^��@@@�������#Lwt&return��� _���� _�@��� _���� _�@@@��@������,int_of_float��� _�
�� _�@��� _�
�� _�@@@��@������/float_of_string��� _��� _�'@��� _��� _�'@@@��@����0ticker_price_str��� _�(�� _�8@��� _�(�� _�8@@@@��� _��� _�9@���� _��� _�8@@@@��� _�	�� _�:@���� _�
�� _�9@@@@��� _���� _�:@@@��� ^���� _�:@@@@��� ^���� _�<@���� ^���� _�:@@@@��� ]���� _�<@@@@��� \���� _�<@@��������� c���� c��@��� c���� c��@@@��@������$user��� c���� c��@��� c���� c��@@@��'balance��� c���� c��@��� c���� c��@@@��@������m�� c��� c��@�� c��� c��@@@��@����,ticker_price�� c��� c��@�� c��� c��@@@��@����!n�� c��� c��@�� c��� c��@@@@��! c���" c��@���% c���& c��@@@@��( c���) c��@@@��+ \���, c��@@@����#int��3 [5�4 [5�@@��6 [5�7 [5�@@@��9 [5}�: c��@@@��< [5s�= c��A@@��? [5b�@ c��A@@��B [5W�C c��A@@@��E [57�F c��@@��H [57�I c��@���@�����#buy��T j&,�U j&/@��W j&,�X j&/@@@��@@�����$user��b j&1�c j&5@��e j&1�f j&5@@@����!t��m j&8�n j&9@@��p j&8�q j&9@@@��s j&0�t j&:@@@��@@�����%index��~ j&<� j&A@��� j&<�� j&A@@@����&string��� j&D�� j&J@@��� j&D�� j&J@@@��� j&;�� j&K@@@��@@�����!n��� j&M�� j&N@��� j&M�� j&N@@@����#int��� j&Q�� j&T@@��� j&Q�� j&T@@@��� j&L�� j&U@@@��������+able_to_buy��� kX_�� kXj@��� kX_�� kXj@@@��@����$user��� kXk�� kXo@��� kXk�� kXo@@@��@����%index��� kXp�� kXu@��� kXp�� kXu@@@��@����!n��� kXv�� kXw@��� kXv�� kXw@@@@��� kX_�� kXw@@@������&stocks��� n���� n��@������5update_new_stock_list��� n���� n��@��� n���� n��@@@��@������$user�� n��� n��@��
 n��� n��@@@��&stocks�� n��� n��@�� n��� n��@@@��@����%index�� n��� n��@��  n���! n��@@@��@����!n��* n���+ n��@��- n���. n��@@@@��0 n���1 n��@@@����'balance��8 o���9 o��@������;subtract_and_update_balance��B o���C o� @��E o���F o� @@@��@����$user��O o��P o�@��R o��S o�@@@��@����%index��\ o��] o�@��_ o��` o�@@@��@����!n��i o��j o�@��l o��m o�@@@@��o o���p o�@@@@�����$user��x m���y m��@��{ m���| m��@@@��~ l}�� p@@@�����$user��� q �� q$@��� q �� q$@@@��� kX\�� q$@@@��� j&L�� q$A@@��� j&;�� q$A@@��� j&0�� q$A@@���z��L@@ ��M@@ �A�������
  M [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought.��� e���� i%@@��� e���� i%@@@@��� e���� i%@@��� e���� i%@@��� j&(�� q$@@��� j&(�� q$@���A�����,able_to_sell��� s&0�� s&<@��� s&0�� s&<@@@��@@�����&stocks��� s&>�� s&D@��� s&>�� s&D@@@����$list��� s&V�� s&Z@��������&string��� s&H�� s&N@@��� s&H�� s&N@@@�����#int��� s&Q�� s&T@@��� s&Q�� s&T@@@@��� s&H�� s&T@@@@��� s&G�� s&Z@@@��� s&=�  s&[@@@��@@�����%index��
 s&]� s&b@�� s&]� s&b@@@����&string�� s&e� s&k@@�� s&e� s&k@@@�� s&\� s&l@@@��@@�����!n��& s&n�' s&o@��) s&n�* s&o@@@����#int��1 s&r�2 s&u@@��4 s&r�5 s&u@@@��7 s&m�8 s&v@@@�  ������&stocks��C u���D u��@��F u���G u��@@@���������O v���P v��@@��R v���S v��@@@@����%false��Z v���[ v��@@��] v���^ v��@@@������ư�f w���g w��@��@�������@��r w���s w��@@@����!i��z w���{ w��@��} w���~ w��@@@@��� w���� w��@���� w���� w��@@@����!t��� w���� w��@��� w���� w��@@@@��� w���� w��A@@��� w���� w��@@@@��������!>��� w���� w��@��� w���� w��@@@��@����!i��� w���� w��@��� w���� w��@@@��@����!n��� w���� w��@��� w���� w��@@@@��� w���� w��@@@����$true��� w���� w��@@��� w���� w��@@@�������,able_to_sell��� w���� w��@��� w���� w��@@@��@����!t��� w���� w��@��� w���� w��@@@��@����%index��� w���� w��@��� w���� w��@@@��@����!n��� w���� w��@�� w��� w��@@@@�� w��� w��@@@�� w��� w��@@@@��
 u��� w��@@@����$bool�� tw� tw�@@�� tw� tw�@@@�� tw}� w��@@@�� s&m� w��A@@�� s&\� w��A@@��! s&=�" w��A@@@��$ s&(�% w��@@��' s&(�( w��@���@�����$sell��3 ~]c�4 ~]g@��6 ~]c�7 ~]g@@@��@@�����$user��A ~]i�B ~]m@��D ~]i�E ~]m@@@����!t��L ~]p�M ~]q@@��O ~]p�P ~]q@@@��R ~]h�S ~]r@@@��@@�����%index��] ~]t�^ ~]y@��` ~]t�a ~]y@@@����&string��h ~]|�i ~]�@@��k ~]|�l ~]�@@@��n ~]s�o ~]�@@@��@@�����!n��y ~]��z ~]�@��| ~]��} ~]�@@@����#int��� ~]��� ~]�@@��� ~]��� ~]�@@@��� ~]��� ~]�@@@������������ ���� ��@��� ���� ��@@@��@������,able_to_sell��� ���� ��@��� ���� ��@@@��@������$user��� ���� ��@��� ���� ��@@@��&stocks��� ���� ��@��� ���� ��@@@��@����%index��� ���� ��@��� ���� ��@@@��@����!n��� ���� ��@��� ���� ��@@@@��� ���� ��@@@��@������� ���� ��@@��� ���� ��@@@@��� ���� ��@@@������&stocks��� ����� ���@������7update_user_stocks_list��� ����  ��@�� ���� ��@@@��@������$user�� ��� ��@�� ��� ��@@@��&stocks�� ��� ��@�� ��� ��@@@��@����%index��$ ���% ��@��' ���( ��@@@��@������	���2 ���3 ��@��5 ���6 ��@@@��@���"-1@��> ���? ��@@@��@����!n��H ���I �� @��K ���L �� @@@@��N ���O ��!@���R ���S �� @@@@��U ����V ��!@@@����'balance��] �#+�^ �#2@������;subtract_and_update_balance��g �#5�h �#P@��j �#5�k �#P@@@��@����$user��t �#Q�u �#U@��w �#Q�x �#U@@@��@����%index��� �#V�� �#[@��� �#V�� �#[@@@��@������	���� �#`�� �#a@��� �#`�� �#a@@@��@���"-1@��� �#]�� �#_@@@��@����!n��� �#b�� �#c@��� �#b�� �#c@@@@��� �#\�� �#d@���� �#]�� �#c@@@@��� �#5�� �#d@@@@�����$user��� ����� ���@��� ����� ���@@@��� ����� �fm@@@�����$user��� �nw�� �n{@��� �nw�� �n{@@@��� ���� �n{@@@��� ~]��� �n{A@@��� ~]s�� �n{A@@��� ~]h�� �n{A@@�������@@ ���@@ �A�������
  b [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold.��� y���� }.\@@��� y���� }.\@@@@��� y���� }.\@@��� y���� }.\@@��� ~]_�� �n{@@��� ~]_�� �n{@���@�����(next_day�� ���� ���@�� ����	 ���@@@��@@�����$user�� ���� ���@�� ���� ���@@@����!t�� ���� ���@@��! ����" ���@@@��$ ����% ���@@@������#day��. ����/ ���@���������7 ����8 ���@��: ����; ���@@@��@������$user��F ����G ���@��I ����J ���@@@��#day��O ����P ���@��R ����S ���@@@��@���!1@��[ ����\ ���@@@@��^ ����_ ���@@@@�����$user��g ����h ���@��j ����k ���@@@��m ����n ���@@@��p ����q ���A@@���T��&@@ ��'@@ �A�������	' Iterates the user by 1 to the next day��� �}�� �}�@@��� �}�� �}�@@@@��� �}�� �}�@@��� �}�� �}�@@��� ����� ���@@��� ����� ���@���@�����0display_username��� ����� �� @��� ����� �� @@@��@@�����$user��� ���� ��@��� ���� ��@@@����!t��� ��	�� ��
@@��� ��	�� ��
@@@��� ���� ��@@@�  ������$user��� ���� ��@��� ���� ��@@@��(username��� ���� ��$@��� ���� ��$@@@����&string��� ���� ��@@��� ���� ��@@@��� ���� ��$@@@��� ���� ��$A@@@��� ����� ��$@@��� ����� ��$@@���f5N�� �%(@@����$User���f5G��f5K@���f5G��f5K@@���f5E�� �%(@@���߰��@@ ���@@ �A�������8 Implementation of User ��e�e4@@��e�e4@@@@��e�e4@@��e�e4@@��f55� �%(@��f55� �%(@@