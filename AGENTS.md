# meguri

購入調査（purchase-research スキル）の決まった作業を受け持つ CLI。販路の一覧を持ち、検索URLを組み立て、ページを取り、値を抜き出して揃え、「どこまで探したか」の表を埋める。判断（同じ物か、状態、本命選び）と、ブラウザでしか見られないサイトを開く作業は、エージェントと人に残す。

買わない。ログインしない。フォームを送らない（カタログに載せた読み取り専用の POST は除く）。自分では判断しない（「探している物か」の判定は、頼まれたときだけ、判定モデル jev か外のコマンドに渡す。文章を作る LLM は呼ばない）。ボット確認やブロックが出たら、そのホストの取得をやめる（UAは `meguri/<版> (+リポジトリのURL)` と名乗り、偽装しない）。

## 開発のコマンド

```bash
nix develop                         # ghc・cabal・hls が入ったシェル
cabal build exe:meguri              # ビルド
cabal test                          # オフラインのテスト（fixture → golden、部品のテスト）
cabal test --test-options=--accept  # 抽出を変えたとき：golden を作り直す（差分を目で確かめる）
cabal run meguri -- sites --format md > docs/channels.md   # カタログを変えたら必ず作り直す
cabal run meguri -- check --write   # 健康診断。結果を src/Meguri/Catalog/Health.hs と docs/channels.md に書く
nix build                           # packages.default
nix flake check                     # テストと、docs/channels.md が古くないかの確認（オフライン）
```

- flake は git が追っているファイルしか見ない。新しいファイルは `git add` してから `nix build` する。
- 依存は、開発シェルが用意した版だけを使う（`cabal.project` の `active-repositories: :none`）。依存を足したら `meguri.cabal` に書いて、シェルに入り直す。
- nix は sdist からビルドする。fixture と golden は `meguri.cabal` の `extra-source-files` に載っている。
- 実際のサイトに当てるのは、手でコマンドを走らせたときだけ。テストと CI では当てない。

## 使い方

人向けの説明は README.md にある。

```bash
meguri search "WH-1000XM5" --min-price 12000 --has XM5     # 何を・どこを → 個別ページまで読んだ結果、を1回で
meguri search "WH-1000XM5" --format jsonl                   # 構造化した出力（meguri.result/1 など）
meguri search "WH-1000XM5" --format md                      # スキルの出力の表（候補・未確認の候補・どこまで探したか）
meguri search "WH-1000XM5" --url <URL>                     # 名指しのページ（HTTPで読める個別ページ）を結果に入れる
meguri search "WH-1000XM5" --judge '<質問>'                  # 何を：探している物かどうかを、判定モデル jev に聞く（OPENROUTER_API_KEY が要る）
meguri search "WH-1000XM5" --judge-cmd '<コマンド>'           # 何を：同じ判定を、外のコマンドに任せる
meguri search "WH-1000XM5" --category リユース --judge-channel '<質問>'   # どこを：当たる販路を選ぶ
MEGURI_PREF=13 meguri search "WH-1000XM5" --only hardoff    # 送料を計算できるサイトで、配送先までの送料を入れる
meguri item <URL>                                           # 個別ページを1つ読み、基準の項目を出どころつきで出す
meguri sold "WH-1000XM5" --min-price 12000                  # ヤフオクの落札相場（件数・最安・中央値・最高）
meguri recall "ヘッドホン" "headphone"                       # 消費者庁と CPSC のリコール情報
meguri sites [--format md]                                  # カタログ
meguri check [--write] [--all] [--only <site>]              # サイトごとの健康診断
```

- **`search` は、入力だけで結果が決まる。** 前に走らせた結果を読んで続きに使うコマンドは無い（`verify`・`rank`・`render` と `--run` は、なくした）。キャッシュは持たないので、走らせ直すとサイトにもう一度当たり、jev にももう一度聞く。
- **`search` の形は「何を（`Query`）→ どこを（`Scope`）→ 結果（`Report`）」。** 「どこまで」は `Effort`。
  - 何を：検索語と、見つけた売り物のうち「探している物」と認める選び方（`--min-price`・`--max-price`・`--has`・`--not`・`--judge`・`--judge-cmd`）。
  - どこを：当たる販路の選び方（`--only`・`--skip`・`--category`・`--judge-channel`・`--judge-channel-cmd`）、新品・中古（`--market`）、名指しのページ（`--url`）。
  - どこまで：個別ページを読む数（`--read N`。販路と新品・中古の組ごとに、安い順で。既定は 3）と、時間（`--time`）。
- **選び方（`Selection a`）は、条件（`Criterion`）と判定（`Judge`）でできている。** 選ばれるのは「全部を満たすもの」で、書く順によらない。条件を先に、判定を後にかける。
  - 条件は純粋で、1件ごとに決まる。「何を」の条件は、一覧の値と、個別ページを読んだあとの値の両方にかける。
  - 判定は、判断を外に任せる（jev か、外のコマンド）。一覧がそろってから1回だけ呼ぶ。失敗したら、そこでやめる（かけなかったことにして先へ進まない）。
  - 名指しのページは、「何を」の選び方で外さない。
  - 「選ぶ」ものを足すときは、何を選ぶのか（販路か、売り物か）を先に決めて、その `Selection` の条件か判定として足す。専用の引数を別に生やさない。
- **jev**（TypeSafe の判定モデル）は、OpenRouter の Decisions API で呼ぶ。`OPENROUTER_API_KEY` があるときだけ動き、無ければ取りに行く前に終了コード 2 でやめる。1件につき1回、はい／いいえの質問をして、「はい」の確率が 0.5 以上なら残す。キーはログに入れない。渡すのは、一覧の値と検索語だけ（配送先は渡さない）。モデルは `MEGURI_JEV_MODEL` で変えられる。
- 選び方が無いと、安い順の上に並ぶ付属品の個別ページを読むことになる。型番なら `--min-price` と `--has`、か `--judge` を付ける。
- **時間の上限は `--time 秒`**（既定は 60）。過ぎたら読めた分だけで返し、読めなかった候補は「時間切れ」として出す。足りなければ `--time` を延ばす（もう一度走らせても、続きからは読まない）。楽天の個別ページは、1件に10秒ほどかかることがある（2026-11-21）。
- 実行の記録は `$XDG_CACHE_HOME/meguri/runs/<id>/`（`readings.jsonl`＝読んだ個別ページの全項目と出どころ、`sightings.jsonl`＝一覧で見かけた売り物の全部、`coverage.jsonl`、`pages/`）。書くだけで、あとのコマンドは読まない。7日で消える。フォルダは実行ごとに別の名前で作る（同じ秒に始まったら `-2`・`-3` を付ける）。
- 標準出力（`table`）は30KB以内。確かめた候補は、枠ごとに、確認済み → 一部 の順で、支払額の安い順。同じURLと同じ在庫（共通の在庫番号）は1つにまとめる。
- `MEGURI_PREF`（配送先の都道府県コード 01〜47 か都道府県名）があると、`search`・`item`・`ship` は、送料を計算できるサイト（ハードオフ・トレジャーファクトリー）で送料を問い合わせる。配送先は、実行の記録に残さない。
- 終了コード：0＝成功（一部のサイトがブロックでも成功）、2＝使い方の誤り、3＝取りに行った全サイトで失敗（か、判定が失敗）。
- JSON Lines の各行には `schema`（`meguri.result/1`・`meguri.unread/1`・`meguri.summary/1`・`meguri.coverage/2`・`meguri.reading/1`・`meguri.sighting/1`・`meguri.sold/1`・`meguri.recall/1`）が付く。

## 作りの要点

```
Request
 ├─ scope.channels ─ カタログから選ぶ ──→ 当たる販路（Selected Channel） ─┐
 ├─ scope.markets ────────────────────────────────────────────────────────┤
 ├─ query.terms ──────────────────────────────────────────────────────────┴─ lookups ──→ [Lookup]
 │                              [Lookup] ─ 取得・抽出・正規化（listThen） ──→ [Coverage]（どこまで探したか）
 │                                                                       └─→ [Sighting]（一覧で見かけた売り物）
 ├─ query.accept ─ [Sighting] から選ぶ ──→ Selected Sighting ─ cheapest ──→ [Page] ─┐
 ├─ scope.pages（名指し） ─────────────────────────────────────────────────→ [Page] ─┴─ readPages ──→ [Checked] + [Unread]
 ├─ query.accept の条件 ─ 読めた値でもう一度 ──→ [Checked]
 └─ arrange（重複の除去・枠） ──→ [Finding] ──→ Report ──→ 出力（table・jsonl・md）
```

- **状態を持たない。** `search` の結果は、`Request` と、そのときのサイトの中身だけで決まる。途中の結果をファイルに置いて、あとのコマンドが書き足していく作りにしない。キャッシュも持たない（2026-11-23 になくした。当たるのは同じ検索をすぐ走らせ直したときだけで、同時に走らせると落ちる元になっていた）。速さは、並列の取得で出す。
- **ブロックされたら、その事実を返す。** ブロックと決めた根拠（状態コード・ページの目印）、ページの題、Server・Retry-After のヘッダ、取った時刻、ページのURLを、「どこまで探したか」と「読めなかった候補」に入れる（`Block`）。ブラウザで保存したHTMLを受け取る機能は持たない（2026-11-23 になくした。`search --page`・`item --html` と、そのためのアダプタ）。ブラウザでしか開けないサイトは、エージェントと人が開く。
- **同時に走らせてよい。** プロセスの外に置くのは、実行の記録（書くだけ）と、ホストごとのロックファイル（`$XDG_CACHE_HOME/meguri/locks/`、中身は空）だけ。どちらも、ほかの meguri とぶつかっても壊れない作りにする。
- **名前**（同じものを、別の名前で呼ばない）：

  | 名前 | 意味 |
  |---|---|
  | `Channel` | 販路（カタログの1件） |
  | `Market` | 新品（`New`）か中古（`Used`）か |
  | `Lookup` | 検索1回（販路・語・新品か中古か） |
  | `Coverage` | 検索1回の結果（どこまで探したか、の1行） |
  | `Offer s` | 売り物1つについて読めた値。`s` は、値をどこで見たか（`OnList`＝一覧・API、`OnPage`＝個別ページ） |
  | `Sighting` | 一覧で見かけた売り物（未確認） |
  | `Page` | 読む個別ページ（一覧から選んだもの、名指しのもの） |
  | `Reading` | 個別ページを読んだ値と、確認済みの判定 |
  | `Checked`・`Unread` | 個別ページを読めたもの・読めなかったもの |
  | `Finding` | 結果の1件（枠つき。重複をまとめたあと） |
  | `Seen` | 選び方が見る、売り物の見え方（`Sighting` からも `Reading` からも作る） |

- **モジュール**：
  - `Meguri.Search`：`search` の全体（`Request`・`Effort`・`Report`）。判定が無ければ、一覧が取れた販路から順に個別ページを読み始める。結果をまとめる `assemble` は純粋。
  - `Meguri.Selection`：選び方（`Selection`・`Criterion`・`Judge`・`Selected`・`Excluded`）。何を選ぶかによらない。
  - `Meguri.Judge`：判定の頼み先。外のコマンドと jev。
  - `Meguri.Query`：何を探すか（`Query`・`Seen`、価格と商品名の条件）。
  - `Meguri.Scope`：どこを探すか（`Scope`、販路の条件）。
  - `Meguri.List`：検索の一覧を作り（`lookups`）、取りに行く。
  - `Meguri.Read`：読むページの選び方（`cheapest`）と、個別ページの読み取り。期限つき。
  - `Meguri.Arrange`：重複を除き、枠に分けて並べる。
  - `Meguri.Report`：`search` の出力（表・JSON Lines・Markdown）。

- **サイトの知識の出どころは `src/Meguri/Catalog.hs` だけ。** `docs/channels.md` はそこから作る。注記を docs に直接書かない。健康診断の結果だけは `src/Meguri/Catalog/Health.hs` にあり、`meguri check --write` が書く（手で直さない）。
- **取得と抽出を分ける。** 抽出は「HTMLの文字列 → 値」の純粋な関数（`Meguri.Extract.*`・`Meguri.Site.*`）。HTTP で取ったページにも、fixture にも同じ関数を使う。取得・時刻・ファイルの読み書きは `Meguri.Fetch.*`・`Meguri.Run`・`app/Main.hs` に置く。
- **値ごとに出どころを持たせる**（`Field s a`）。読めなかった値は `null` にして `note` に理由を書く。数字は作らない：税抜しか無ければ税込は `null`、本文で税込と確かめられない額も `null`、送料が配送先しだいなら支払額は `null`。
  - 価格の欄に「税込」の文字が無い店（コストコ）は、確かめた日つきの「サイトの決まり」として税込と読み、そのことを `note` に書く。
  - BUYMA は、「関税負担なし」の印があるときだけ、表示価格を支払う額として読む。
- **確認済みは型で縛る。** 一覧・APIの値は `Offer OnList`、個別ページの値は `Offer OnPage`。`Meguri.Confirm.judge` は後者しか受け取らないので、一覧の値から `Confirmed` は作れない。汎用の抽出だけ（アダプタなし）の記録も `Confirmed` にしない。枠に分けるとき（`Meguri.Arrange`）も、一覧の候補は未確認のまま並べる。
- **汎用の部品が先、アダプタは癖だけ。** `Meguri.Extract.Generic` が JSON-LD → microdata（Product の範囲だけ）→ OGP の順に読み、アダプタが上書きする。
  - `Meguri.Site.<サイト>`：モール・フリマ・リユース店（楽天、Yahoo!、価格.com、ヤフオク、Yahoo!フリマ、ラクマ、ハードオフ、トレジャーファクトリー、ジモティー、ブックオフ）。
  - `Meguri.Site.DirectStores`：直販の店（ロフト、ハンズ、Francfranc、コーナン、ディノス、ジャパネット、イオシス、イシバシ楽器、カインズ、cado、買取王国、赤ちゃん本舗、コストコ、IKEA、ソフマップ）。共通の部品は `Meguri.Site.Store`（税込の裏づけ、「◯円以上で送料無料」、カートの読み取り、在庫の言い回し、JAN のチェックディジット、一覧のカード切り）。
  - `Meguri.Site.Malls`：店や出品者が入れ替わる売り場（LOHACO、au PAY マーケット、BUYMA、ロコンド）。販売者は、ページに書いてある店・出品者の名前を読む。
- **出力する文字列は整える**（`Meguri.Sanitize`）。商品名や店名はページの書き手が自由に書ける。長さを切り、制御文字を除き、URL は http(s) だけを通す。説明文は抜き出さない。ページに書かれたURLをたどるときは、公開のホストだけを取りに行く。
- **取得の決まり**（`Meguri.Fetch.Http`）：同じホストへは同時に1本・間隔2秒以上で、ほかの meguri のプロセスともロックファイル（flock）で分け合う。取り終わってから2秒ロックを持ち続けて放すので、前の取得の時刻はどこにも書かない。全体の同時数は、プロセスごとに8。応答はキャッシュしない。403・429・ボット確認・Access Denied は `Blocked` として、その事実（`Block`）を返し、そのホストをそこでやめる。検索結果は1ページ目だけを取る。続きのページ（価格.com のシリーズ → 色ごとのショップ一覧）は、1つの検索につき4ページまで。
- フィールド名がぶつかるので `NoFieldSelectors` を使っている。値は `x.field` で読む。

## アダプタを足すとき

1. `Catalog.hs` のサイトの定義を確かめる（段・検索URL・個別ページのURLの形・注記・確かめた日・健康診断の見本の語）。個別ページのURLの形は、商品のページだけに当たるよう狭く書く（`check` が最初に当たったリンクを読む）。
2. 個別ページの要点を調べる：JSON-LD の中身、カートのボタンの目印、送料・税込・在庫の表示。直販の店なら `DirectStores` に店の決まりを足す。
3. ログインしていない状態で取ったページを、要る部分だけに切り詰めて `test/fixtures/<site>/` に置く。出品者などの個人の名前と、配達先の地名・郵便番号は入れない。
4. `test/Main.hs` の `goldenTests` に足し、`cabal test --test-options=--accept` で golden を作って、中身を目で確かめる。`--accept` は既存の golden も黙って書き換えるので、`git diff test/golden` で変わったものを見る。
5. `meguri check --only <site>` で、実際のサイトでも同じように読めるかを見る。
6. 一覧のアダプタは、その店に無い語（型番など）でも `meguri search "<語>" --only <site>` を走らせて、0件になるかを見る。検索結果の後ろや、該当なしのページに、おすすめ・ランキング・最近売れた商品の枠を並べるサイトがある（ハンズ、ディノス、BUYMA）。検索結果そのものをJSで描画し、HTMLにはそうした枠しか無いサイトもある（Francfranc）。読む範囲を検索結果の入れ物に限る（`CardOpts` の `regionId`・`emptyMarks`）。

## いまの範囲

- 検索と個別ページ：楽天（支店：ジョーシン・エディオン・2nd STREET）、Yahoo!ショッピング（2nd STREET の Yahoo!店）、ヤフオク（検索・落札相場・個別）、Yahoo!フリマ、ラクマ、ハードオフ、トレジャーファクトリー、ジモティー、ブックオフ、ロフト、ハンズ、コーナン、ディノス、ジャパネット、イオシス、イシバシ楽器、カインズ、cado、買取王国、赤ちゃん本舗、BUYMA。価格.com は検索とショップ一覧（個別ページとしては読まない）。
- 個別ページだけ：コストコ、IKEA、ソフマップ（新品）、Francfranc、LOHACO、au PAY マーケット、ロコンド。
- ほかのサイトは、開くべきURLの行として「どこまで探したか」に出る。`item` は汎用の抽出で読める分だけ読み、`partial` にとどめる。
- まだ無いもの：
  - HTTP の段で、まだ汎用の抽出だけのサイト：ショップチャンネル、BALMUDA、plywood、ACTUS、unico、IDÉE、ビバホーム、たのめーる、カウネット、モバオク、ワンダーレックス、Rentio、CLAS、honto、紀伊國屋書店（`check` の「汎用の抽出」の行）。
  - au PAY マーケットの送料とカートのボタン（JSで描画される）は読めない。
  - 公式API（Yahoo!ショッピング 商品検索 v3 など）と、meguri がブラウザを動かす取得（指示書の 2.5 の Phase 2。任意）。
  - 送料の計算は2サイトだけ。価格.com の転送URL（アフィリエイトの中継を通るもの）は、自動では開かない（`Meguri.Read.tracked`）。「ブラウザで開く」として出す。
  - 応答の遅いホスト（楽天の個別ページ）は、同じホストへ1本ずつという決まりのため、1回の `search` では読み切れないことがある。

## Phase 0 で調べて決めたこと（2026-11-20）

- **UA**：`meguri/<版> (+https://github.com/yuidvg/meguri)`。URL は `Meguri.Fetch.Http.repoUrl` にある。リポジトリは 2026-11-22 に公開したので、サイト側からこのURLを開ける。
- **段の確かめ直し**：HTTP可・個別ページだけHTTP可の段のサイトに、このUAで検索1回・個別1件を当てた。403 やボット確認は出なかった。いまは `meguri check` が同じことをして、結果を channels.md の「診断」に入れる。
- **TLS**：hands.net は Extended Main Secret に対応していない。tls の既定（必須）ではつながらないので、「あれば使う」設定にした（証明書の検証は外していない）。
- **公式API**（まだ実装していない。鍵の値はログに入れない）

  | API | 決めたこと | 環境変数 |
  |---|---|---|
  | Yahoo!ショッピング 商品検索 v3 | 使う。JAN・在庫・送料区分・中古の絞り込みが取れる。送料の額は返らない | `MEGURI_YAHOO_CLIENT_ID` |
  | 楽天市場 商品検索 | 鍵があるときだけ使う。開発者ポータルが 11/9 から停止中で、新しい鍵は取れない。中古の絞り込みと送料の額は無いので、HTMLの検索を残す | `MEGURI_RAKUTEN_APP_ID`・`MEGURI_RAKUTEN_ACCESS_KEY` |
  | eBay Browse | 鍵があるときだけ使う。JPY では返らない見込み。申請なしで本番の検索が通るかは未確認 | `MEGURI_EBAY_CLIENT_ID`・`MEGURI_EBAY_CLIENT_SECRET` |
  | Amazon | 使わない（Creators API は、アソシエイトの承認と直近の成果が前提）。ブラウザの段のまま | — |

- **agent-browser**（0.38.2）：ヘッドレスでも画面ありでも、開いたページのHTMLを `eval "document.documentElement.outerHTML"` でファイルに書き出せる。JSで描画される検索（LOHACO・au PAY・コストコ・ソフマップ・IKEA）は、ヘッドレスで開いて商品リンクを拾えた。Chrome を起動しているときに `--profile Default` を使えるかは、まだ確かめていない。
