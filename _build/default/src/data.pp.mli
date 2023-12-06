Caml1999N032����            ,src/data.mli����  �       D�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����&stocks��.<command-line>A@A�A@G@@��A@@�A@H@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text���@@ ���@@ �A�������	K Module providing functions for retrieving financial data and information. ��,src/data.mliA@@�A@ P@@��A@@�A@ P@@@@��A@@�A@ P@@��
A@@�A@ P@��A@@�A@ P@�����$Data��C R ^�C R b@������A�  # �+stock_query��#D i p�$D i {@@@@A��������&string��/D i ~�0D i �@@��2D i ~�3D i �@@@�����#int��;D i ��<D i �@@��>D i ��?D i �@@@@��AD i ~�BD i �@@@@��DD i k�ED i �@@��GD i k�HD i �@���Р(get_date��PF � ��QF � �@��@����$unit��ZF � ��[F � �@@��]F � ��^F � �@@@����&string��eF � ��fF � �@@��hF � ��iF � �@@@��kF � ��lF � �@@@@���)ocaml.doc��*@@ ��+@@ �A�������	? [get_date ()] returns the current date as a formatted string. ��}G � ��~G � �@@���G � ���G � �@@@@���G � ���G � �@@���G � ���G � �@@���F � ���F � �@���F � ���F � �@���Р0get_ticker_price���I � ���I �
@��@����+stock_query���I ���I �@@���I ���I �@@@�����#Lwt!t���I �#��I �(@�����&string���I ���I �"@@���I ���I �"@@@@���I ���I �(@@@���I ���I �(@@@@���S��|@@ ��}@@ �A�������	x [get_ticker_price symbol] returns a promise (Lwt.t) of the latest ticker
      price for the specified stock [symbol]. ���J)+��Kx�@@���J)+��Kx�@@@@���J)+��Kx�@@���J)+��Kx�@@���I � ���I �(@���I � ���I �(@���Р;calculate_stock_correlation���M����M��@��@����+stock_query���M����M��@@���M����M��@@@��@����+stock_query���M����M��@@��M���M��@@@��@����#int��M���M��@@��M���M��@@@�����#Lwt!t��M���M��@�����%float��!M���"M��@@��$M���%M��@@@@��'M���(M��@@@��*M���+M��@@@��-M���.M��@@@��0M���1M��@@@@���Ű��@@ ���@@ �A�������	� [calculate_stock_correlation symbol1 symbol2 days] returns a promise
      (Lwt.t) of the correlation coefficient between two stocks represented by
      [symbol1] and [symbol2] over the specified number of [days]. ��AN��BP��@@��DN��EP��@@@@��GN��HP��@@��JN��KP��@@��MM���NM��@��PM���QM��@���Р5get_latest_news_feeds��YR���ZR��@��@����+stock_query��cR���dR�	@@��fR���gR�	@@@�����#Lwt!t��pR�,�qR�1@�����$list��yR�'�zR�+@��������&string���R���R�@@���R���R�@@@�����&string���R���R�@@���R���R�@@@�����%float���R� ��R�%@@���R� ��R�%@@@@���R���R�%@@@@���R���R�+@@@@���R���R�1@@@���R����R�1@@@@���A��j@@ ��k@@ �A�������	� [get_latest_news_feeds symbol] returns a promise (Lwt.t) of a list
      containing the latest news feeds for the specified stock [symbol]. Each
      news item is represented as a tuple (title, summary, sentiment_score). ���S24��U�@@���S24��U�@@@@���S24��U�@@���S24��U�@@���R����R�1@���R����R�1@���Р6generate_stock_summary���W ��W6@��@����+stock_query���W9��WD@@���W9��WD@@@�����#Lwt!t���WO��WT@�����&string���WH��WN@@���WH��WN@@@@���WH��WT@@@���W9��WT@@@@�������@@ ���@@ �A�������	� [generate_stock_summary symbol] returns a promise (Lwt.t) of a summary for
      the specified stock [symbol], including relevant information such as
      current price, price a year ago, high and low prices, and average trading
      volume. ��XUW�[AQ@@��XUW�[AQ@@@@��XUW�[AQ@@��XUW�[AQ@@��W�WT@��W�WT@@��!C R e�"\RU@@@��$C R R�%\RU@��'C R R�(\RU@������'DataAPI��1^W^�2^We@����$Data��9^Wh�:^Wl@��<^Wh�=^Wl@@���Ѱ��@@ ���@@ �A�������9 Implementation of Data. ��M_mm�N_m�@@��P_mm�Q_m�@@@@��S_mm�T_m�@@��V_mm�W_m�@@��Y^WW�Z^Wl@��\^WW�]^Wl@@