Caml1999N032����            +src/cli.mli����  (A  +  x  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����&stocks��.<command-line>A@A�A@G@@��A@@�A@H@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text���@@ ���@@ �A�������	T Module providing functions for user interactions and financial data
    retrieval. ��+src/cli.mliA@@�B H Y@@��A@@�B H Y@@@@��A@@�B H Y@@��
A@@�B H Y@��A@@�B H Y@������$User��D [ `�D [ d@A��D [ [�D [ d@@��D [ [�D [ d@������$Data��'E e j�(E e n@A��*E e e�+E e n@@��-E e e�.E e n@�����'CliType��6G p |�7G p �@���������)User_Impl��CH � ��DH � �@����(UserImpl��KH � ��LH � �@��NH � ��OH � �@@���)ocaml.doc��@@ ��@@ �A�������	t Module type representing the User module, providing functions for user
      interactions with a financial system. ��`I � ��aJ �#@@��cI � ��dJ �#@@@@��fI � ��gJ �#@@��iI � ��jJ �#@@��lH � ��mH � �@��oH � ��pH � �@������)Data_Impl��yL%.�zL%7@����'DataAPI���L%:��L%A@���L%:��L%A@@���6��B@@ ��C@@ �A�������	t Module type representing the Data module, providing functions for
      retrieving financial data and information. ���MBD��N��@@���MBD��N��@@@@���MBD��N��@@���MBD��N��@@���L%'��L%A@���L%'��L%A@���A�  # �+stock_query���P����P��@@@@A��������&string���P����P��@@���P����P��@@@�����#int���P����P��@@���P����P��@@@@���P����P��@@@@���P����P��@@���P����P��@���Р)make_user���R����R��@��@����&string���R����R��@@���R����R��@@@��@����%float���R����R�@@���R����R�@@@��@����$bool���R�� R�@@��R��R�@@@�����)User_Impl!t��R��R�@@��R��R�@@@��R��R�@@@��R���R�@@@��R���R�@@@@���ʰ��@@ ���@@ �A�������	� [make_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. ��)S�*U��@@��,S�-U��@@@@��/S�0U��@@��2S�3U��@@��5R���6R�@��8R���9R�@���Р'deposit��AW���BW��@��@�����)User_Impl!t��MW���NW��@@��PW���QW��@@@��@����%float��ZW���[W��@@��]W���^W��@@@�����)User_Impl!t��gW���hW�@@��jW���kW�@@@��mW���nW�@@@��pW���qW�@@@@���"��.@@ ��/@@ �A�������	? [deposit user n] increases the user's balance by [n] dollars. ���X��XH@@���X��XH@@@@���X��XH@@���X��XH@@���W����W�@���W����W�@���Р(withdraw���ZJP��ZJX@��@�����)User_Impl!t���ZJ[��ZJf@@���ZJ[��ZJf@@@��@����%float���ZJj��ZJo@@���ZJj��ZJo@@@�����)User_Impl!t���ZJs��ZJ~@@���ZJs��ZJ~@@@���ZJj��ZJ~@@@���ZJ[��ZJ~@@@@���z���@@ ���@@ �A�������	@ [withdraw user n] decreases the user's balance by [n] dollars. ���[���[�@@���[���[�@@@@���[���[�@@���[���[�@@���ZJL��ZJ~@���ZJL��ZJ~@���Р#buy���]����]��@��@�����)User_Impl!t���]����]��@@�� ]���]��@@@��@����&string��
]���]��@@��]���]��@@@��@����#int��]���]��@@��]���]��@@@�����)User_Impl!t��$]���%]��@@��']���(]��@@@��*]���+]��@@@��-]���.]��@@@��0]���1]��@@@@������@@ ���@@ �A�������
  N [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought. ��A^ �Bb2U@@��D^ �Eb2U@@@@��G^ �Hb2U@@��J^ �Kb2U@@��M]���N]��@��P]���Q]��@���Р$sell��YdW]�ZdWa@��@�����)User_Impl!t��edWd�fdWo@@��hdWd�idWo@@@��@����&string��rdWs�sdWy@@��udWs�vdWy@@@��@����#int��dW}��dW�@@���dW}��dW�@@@�����)User_Impl!t���dW���dW�@@���dW���dW�@@@���dW}��dW�@@@���dWs��dW�@@@���dWd��dW�@@@@���J��V@@ ��W@@ �A�������
  c [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold. ���e����i��@@���e����i��@@@@���e����i��@@���e����i��@@���dWY��dW�@���dWY��dW�@���Р(next_day���k���k�
@��@�����)User_Impl!t���k���k�@@���k���k�@@@�����)User_Impl!t���k���k�'@@���k���k�'@@@���k���k�'@@@@�������@@ ���@@ �A�������	9 [next_day user] iterates the user by 1 to the next day. ���l(*��l(h@@���l(*��l(h@@@@���l(*��l(h@@���l(*��l(h@@���k����k�'@�� k���k�'@���Р.view_portfolio��	njp�
nj~@��@�����)User_Impl!t��nj��nj�@@��nj��nj�@@@����$list�� nj��!nj�@��������&string��,nj��-nj�@@��/nj��0nj�@@@�����#int��8nj��9nj�@@��;nj��<nj�@@@@��>nj��?nj�@@@@��Anj��Bnj�@@@��Dnj��Enj�@@@@������@@ ��@@ �A�������	T [view_portfolio user] returns an (string, int) list of the user's
      portfolio. ��Uo���Vp��@@��Xo���Yp��@@@@��[o���\p��@@��^o���_p��@@��anjl�bnj�@��dnjl�enj�@���Р,view_balance��mr�nr@��@�����)User_Impl!t��yr�zr!@@��|r�}r!@@@����%float���r%��r*@@���r%��r*@@@���r��r*@@@@���<��H@@ ��I@@ �A�������	< [view_balance user] returns a float of the user's balance. ���s+-��s+n@@���s+-��s+n@@@@���s+-��s+n@@���s+-��s+n@@���r��r*@���r��r*@���Р+view_ledger���upv��up�@��@�����)User_Impl!t���up���up�@@���up���up�@@@����#ref���up���up�@�����$list���up���up�@������)User_Impl,ledger_entry���up���up�@@���up���up�@@@@���up���up�@@@@���up���up�@@@���up���up�@@@@�������@@ ���@@ �A�������	C [view_ledger user] returns a reference to the ledger of the user. ���v����v��@@���v����v��@@@@��v���v��@@��v���v��@@��upr�up�@��
upr�up�@���Р;calculate_stock_correlation��x�	�x�	 @��@�����)User_Impl!t��y	#	'� y	#	2@@��"y	#	'�#y	#	2@@@��@����&string��,y	#	6�-y	#	<@@��/y	#	6�0y	#	<@@@��@����&string��9y	#	@�:y	#	F@@��<y	#	@�=y	#	F@@@��@����#int��Fy	#	J�Gy	#	M@@��Iy	#	J�Jy	#	M@@@����%float��Qy	#	Q�Ry	#	V@@��Ty	#	Q�Uy	#	V@@@��Wy	#	J�Xy	#	V@@@��Zy	#	@�[y	#	V@@@��]y	#	6�^y	#	V@@@��`y	#	'�ay	#	V@@@@�����@@ ��@@ �A�������	� [calculate_stock_correlation symbol1 symbol2 days] calculates the
      correlation coefficient between two stocks represented by [symbol1] and
      [symbol2] over the specified number of [days]. ��qz	W	Y�r|	�
$@@��tz	W	Y�u|	�
$@@@@��wz	W	Y�x|	�
$@@��zz	W	Y�{|	�
$@@��}x�	�~y	#	V@���x�	��y	#	V@���Р5get_latest_news_feeds���~
&
,��~
&
A@��@�����)User_Impl!t���~
&
D��~
&
O@@���~
&
D��~
&
O@@@��@����&string���~
&
S��~
&
Y@@���~
&
S��~
&
Y@@@����&string���~
&
]��~
&
c@@���~
&
]��~
&
c@@@���~
&
S��~
&
c@@@���~
&
D��~
&
c@@@@���h��t@@ ��u@@ �A�������	� [get_latest_news_feeds symbol] returns a formatted string containing the
      latest news feeds for the specified stock symbol. ���
d
f�� @
�
�@@���
d
f�� @
�
�@@@@���
d
f�� @
�
�@@���
d
f�� @
�
�@@���~
&
(��~
&
c@���~
&
(��~
&
c@���Р6generate_stock_summary��� B
�
��� B
�@��@�����)User_Impl!t��� B
��� B
�@@��� B
��� B
�@@@��@����&string��� B
��� B
�#@@��� B
��� B
�#@@@����&string�� B
�'� B
�-@@�� B
�'� B
�-@@@��	 B
��
 B
�-@@@�� B
�� B
�-@@@@�������@@ ���@@ �A�������	� [generate_stock_summary symbol] generates a summary for the specified
      stock symbol, including relevant information such as price, volume, and
      news sentiment. �� C.0� E��@@��  C.0�! E��@@@@��# C.0�$ E��@@��& C.0�' E��@@��) B
�
��* B
�-@��, B
�
��- B
�-@@��/G p ��0 F��@@@��2G p p�3 F��@��5G p p�6 F��@������#Cli��? H���@ H��@����'CliType��G H���H H��@��J H���K H��@@������@@ ��	@@ �A�������; Implementation of CliType ��[ I���\ I�@@��^ I���_ I�@@@@��a I���b I�@@��d I���e I�@@��g H���h H��@��j H���k H��@@