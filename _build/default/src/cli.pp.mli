Caml1999N032����            +src/cli.mli����  %�  �  �  8�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����&stocks��.<command-line>A@A�A@G@@��A@@�A@H@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text���@@ ���@@ �A�������	T Module providing functions for user interactions and financial data
    retrieval. ��+src/cli.mliA@@�B H Y@@��A@@�B H Y@@@@��A@@�B H Y@@��
A@@�B H Y@��A@@�B H Y@������$User��D [ `�D [ d@A��D [ [�D [ d@@��D [ [�D [ d@������$Data��'E e j�(E e n@A��*E e e�+E e n@@��-E e e�.E e n@�����'CliType��6G p |�7G p �@���������)User_Impl��CH � ��DH � �@����$User��KH � ��LH � �@��NH � ��OH � �@@���)ocaml.doc��@@ ��@@ �A�������	t Module type representing the User module, providing functions for user
      interactions with a financial system. ��`I � ��aJ �@@��cI � ��dJ �@@@@��fI � ��gJ �@@��iI � ��jJ �@@��lH � ��mH � �@��oH � ��pH � �@������)Data_Impl��yL!*�zL!3@����$Data���L!6��L!:@���L!6��L!:@@���6��B@@ ��C@@ �A�������	t Module type representing the Data module, providing functions for
      retrieving financial data and information. ���M;=��N��@@���M;=��N��@@@@���M;=��N��@@���M;=��N��@@���L!#��L!:@���L!#��L!:@���Р)make_user���P����P��@��@����&string���P����P��@@���P����P��@@@��@����%float���P����P��@@���P����P��@@@�����)User_Impl!t���P����P��@@���P����P��@@@���P����P��@@@���P����P��@@@@�������@@ ���@@ �A�������	� [make_user username balance] creates a new user account with the given
      [username] and initial [balance]. This user starts with an empty portfolio
      and day 0. ���Q����S��@@���Q����S��@@@@���Q����S��@@���Q����S��@@���P����P��@���P����P��@���Р'deposit��U���U��@��@�����)User_Impl!t��U���U��@@��U���U��@@@��@����%float��U���U��@@��U��� U��@@@�����)User_Impl!t��)U���*U��@@��,U���-U��@@@��/U���0U��@@@��2U���3U��@@@@������@@ ���@@ �A�������	? [deposit user n] increases the user's balance by [n] dollars. ��CV���DV�@@��FV���GV�@@@@��IV���JV�@@��LV���MV�@@��OU���PU��@��RU���SU��@���Р(withdraw��[X�\X&@��@�����)User_Impl!t��gX)�hX4@@��jX)�kX4@@@��@����%float��tX8�uX=@@��wX8�xX=@@@�����)User_Impl!t���XA��XL@@���XA��XL@@@���X8��XL@@@���X)��XL@@@@���<��H@@ ��I@@ �A�������	@ [withdraw user n] decreases the user's balance by [n] dollars. ���YMO��YM�@@���YMO��YM�@@@@���YMO��YM�@@���YMO��YM�@@���X��XL@���X��XL@���Р#buy���[����[��@��@�����)User_Impl!t���[����[��@@���[����[��@@@��@����&string���[����[��@@���[����[��@@@��@����#int���[����[��@@���[����[��@@@�����)User_Impl!t���[����[��@@���[����[��@@@���[����[��@@@���[����[��@@@���[����[��@@@@�������@@ ���@@ �A�������
  N [buy user index n] user [user] buys [n] shares of a stock of index
      [index]. It deducts the cost from the user's balance and updates their
      stock portfolio. Returns either the original [user] as the input if the
      stock could not be bought. Returns the edited user with bought stock if
      the stock could be bought. ��\���` #@@��\���` #@@@@��	\���
` #@@��\���` #@@��[���[��@��[���[��@���Р$sell��b%+�b%/@��@�����)User_Impl!t��'b%2�(b%=@@��*b%2�+b%=@@@��@����&string��4b%A�5b%G@@��7b%A�8b%G@@@��@����#int��Ab%K�Bb%N@@��Db%K�Eb%N@@@�����)User_Impl!t��Nb%R�Ob%]@@��Qb%R�Rb%]@@@��Tb%K�Ub%]@@@��Wb%A�Xb%]@@@��Zb%2�[b%]@@@@�����@@ ��@@ �A�������
  c [sell user index n] user [user] sells [n] shares of a stock of index
      [index]. It adds to the user's balance and updates their stock portfolio.
      Returns either the original [user] as the input if the stock could not be
      sold (trying to sell stock they don't have). Returns the edited user with
      sold stock if the stock could be sold. ��kc^`�lg��@@��nc^`�og��@@@@��qc^`�rg��@@��tc^`�ug��@@��wb%'�xb%]@��zb%'�{b%]@���Р(next_day���i����i��@��@�����)User_Impl!t���i����i��@@���i����i��@@@�����)User_Impl!t���i����i��@@���i����i��@@@���i����i��@@@@���T��`@@ ��a@@ �A�������	9 [next_day user] iterates the user by 1 to the next day. ���j����j�6@@���j����j�6@@@@���j����j�6@@���j����j�6@@���i����i��@���i����i��@���Р.view_portfolio���l8>��l8L@��@�����)User_Impl!t���l8O��l8Z@@���l8O��l8Z@@@����$list���l8m��l8q@��������&string���l8_��l8e@@���l8_��l8e@@@�����#int���l8h��l8k@@���l8h��l8k@@@@�� l8_�l8k@@@@��l8^�l8q@@@��l8O�l8q@@@@�������@@ ���@@ �A�������	T [view_portfolio user] returns an (string, int) list of the user's
      portfolio. ��mrt�n��@@��mrt�n��@@@@��mrt�n��@@�� mrt�!n��@@��#l8:�$l8q@��&l8:�'l8q@���Р,view_balance��/p���0p��@��@�����)User_Impl!t��;p���<p��@@��>p���?p��@@@����%float��Fp���Gp��@@��Ip���Jp��@@@��Lp���Mp��@@@@������
@@ ��@@ �A�������	< [view_balance user] returns a float of the user's balance. ��]q���^q�<@@��`q���aq�<@@@@��cq���dq�<@@��fq���gq�<@@��ip���jp��@��lp���mp��@���Р+view_ledger��us>D�vs>O@��@�����)User_Impl!t���s>R��s>]@@���s>R��s>]@@@����#ref���s>}��s>�@�����$list���s>x��s>|@������)User_Impl,ledger_entry���s>a��s>w@@���s>a��s>w@@@@���s>a��s>|@@@@���s>a��s>�@@@���s>R��s>�@@@@���^��j@@ ��k@@ �A�������	C [view_ledger user] returns a reference to the ledger of the user. ���t����t��@@���t����t��@@@@���t����t��@@���t����t��@@���s>@��s>�@���s>@��s>�@���Р;calculate_stock_correlation���v����v��@��@����&string���v����v��@@���v����v��@@@��@����&string���v����v�	@@���v����v�	@@@��@����#int���v�	��v�	@@���v�	��v�	@@@����%float��v�	�v�	@@��v�	�v�	@@@��
v�	�v�	@@@��v���v�	@@@��v���v�	@@@@���°��@@ ���@@ �A�������	� [calculate_stock_correlation symbol1 symbol2 days] calculates the
      correlation coefficient between two stocks represented by [symbol1] and
      [symbol2] over the specified number of [days]. ��!w		�"y	�	�@@��$w		�%y	�	�@@@@��'w		�(y	�	�@@��*w		�+y	�	�@@��-v���.v�	@��0v���1v�	@���Р5get_latest_news_feeds��9{	�	��:{	�	�@��@����&string��C{	�	��D{	�
@@��F{	�	��G{	�
@@@����&string��N{	�
	�O{	�
@@��Q{	�
	�R{	�
@@@��T{	�	��U{	�
@@@@�����@@ ��@@ �A�������	� [get_latest_news_feeds symbol] returns a formatted string containing the
      latest news feeds for the specified stock symbol. ��e|

�f}
_
�@@��h|

�i}
_
�@@@@��k|

�l}
_
�@@��n|

�o}
_
�@@��q{	�	��r{	�
@��t{	�	��u{	�
@���Р6generate_stock_summary��}
�
��~
�
�@��@����&string���
�
���
�
�@@���
�
���
�
�@@@����&string���
�
���
�
�@@���
�
���
�
�@@@���
�
���
�
�@@@@���J��V@@ ��W@@ �A�������	� [generate_stock_summary symbol] generates a summary for the specified
      stock symbol, including relevant information such as price, volume, and
      news sentiment. ��� @
�
��� Be}@@��� @
�
��� Be}@@@@��� @
�
��� Be}@@��� @
�
��� Be}@@���
�
���
�
�@���
�
���
�
�@@���G p ��� C~�@@@���G p p�� C~�@���G p p�� C~�@������#Cli��� E���� E��@����'CliType��� E���� E��@��� E���� E��@@�������@@ ���@@ �A�������; Implementation of CliType ��� F���� F��@@��� F���� F��@@@@��� F���� F��@@��� F���� F��@@��� E���� E��@��� E���� E��@@