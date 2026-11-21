# meguri

購入調査（purchase-research スキル）の決まった作業を受け持つ CLI。販路の一覧を持ち、検索URLを組み立て、ページを取り、値を抜き出して揃え、「どこまで探したか」の表を埋める。判断（同じ物か、状態、本命選び）と、ブラウザでしか見られないサイトを開く作業は、エージェントと人に残す。

買わない。ログインしない。フォームを送らない（カタログに載せた読み取り専用の POST は除く）。LLM を呼ばない。ボット確認やブロックが出たら、そのホストの取得をやめる（UAは `meguri/<版> (+リポジトリのURL)` と名乗り、偽装しない）。

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

```bash
meguri search "WH-1000XM5" --kind all            # 全サイトを当たる。候補（未確認）と「どこまで探したか」
meguri search "WH-1000XM5" --min-price 12000     # 付属品を標準出力から外す（candidates.jsonl には全部残る）
meguri verify --top 10 --min-price 12000         # 候補の上位の個別ページを読み、確認済みかどうかと、一覧からの変化を出す
meguri item <URL>                                # 個別ページを1つ読み、基準の項目を出どころつきで出す
meguri item <URL> --html saved.html              # ブラウザで保存したHTMLを読む（取りに行かない）
MEGURI_PREF=13 meguri ship <URL>                 # 送料を計算できるサイトで、配送先までの送料を入れる
meguri rank --min-price 12000                    # 重複を除き、枠（すぐ買える／入札次第／訳あり／中古／いまは買えない）に分ける
meguri sold "WH-1000XM5" --min-price 12000       # ヤフオクの落札相場（件数・最安・中央値・最高）
meguri recall "ヘッドホン" "headphone"            # 消費者庁と CPSC のリコール情報
meguri render --format md --min-price 12000      # スキルの出力の表（候補・未確認の候補・どこまで探したか）
meguri sites [--format md]                       # カタログ
meguri check [--write] [--all] [--only <site>]   # サイトごとの健康診断
```

- 流れ：`search` → 「ブラウザで開く」の行のURLを開く → `verify`（HTTPで読めるもの）と `item --html`（ブラウザで保存したもの） → `rank` → `render`。
- 実行の状態は `$XDG_CACHE_HOME/meguri/runs/<id>/`（`candidates.jsonl`・`coverage.jsonl`・`items.jsonl`・`sold.jsonl`・`recalls.jsonl`・`ranked.jsonl`・`pages/`）。7日で消える。`search` が新しい実行を作り、ほかのコマンドは直前の実行（か `--run <id>`）を使う。詳しい中身は jq で読む。
- 標準出力は30KB以内。候補は「少なくともこの額」（支払額。送料が分からなければ税込価格）の安い順で、同じURLと同じ在庫（共通の在庫番号）は1つにまとめる。
- `MEGURI_PREF`（配送先の都道府県コード 01〜47 か都道府県名）があると、`item`・`verify`・`ship` は、送料を計算できるサイト（ハードオフ・トレジャーファクトリー）で送料を問い合わせる。配送先は、キャッシュにも実行のフォルダにも残さない。
- 終了コード：0＝成功（一部のサイトがブロックでも成功）、2＝使い方の誤り、3＝取りに行った全サイトで失敗。
- JSON Lines の各行には `schema`（`meguri.candidate/1`・`meguri.coverage/1`・`meguri.item/1`・`meguri.sold/1`・`meguri.recall/1`・`meguri.ranked/1`）が付く。

## 作りの要点

```
カタログ → 検索URL → 取得（間隔・キャッシュ・文字コード） → 抽出 → 正規化 → 候補（未確認）
                                                              └→ どこまで探したか
候補 → 個別ページ（HTTP か、ブラウザで保存したHTML） → 同じ抽出 → 確認済みの判定 → 重複の除去・枠 → 表
```

- **サイトの知識の出どころは `src/Meguri/Catalog.hs` だけ。** `docs/channels.md` はそこから作る。注記を docs に直接書かない。健康診断の結果だけは `src/Meguri/Catalog/Health.hs` にあり、`meguri check --write` が書く（手で直さない）。
- **取得と抽出を分ける。** 抽出は「HTMLの文字列 → 値」の純粋な関数（`Meguri.Extract.*`・`Meguri.Site.*`）。HTTP で取ったページにも、保存したHTMLにも、fixture にも同じ関数を使う。取得・時刻・キャッシュは `Meguri.Fetch.*`・`Meguri.Run`・`app/Main.hs` に置く。
- **値ごとに出どころを持たせる**（`Field s a`）。読めなかった値は `null` にして `note` に理由を書く。数字は作らない：税抜しか無ければ税込は `null`、本文で税込と確かめられない額も `null`、送料が配送先しだいなら支払額は `null`。
- **確認済みは型で縛る。** 一覧・APIの値は `Offer ListSource`、個別ページの値は `Offer ItemSource`。`Meguri.Confirm.judge` は後者しか受け取らないので、一覧の値から `Confirmed` は作れない。汎用の抽出だけ（アダプタなし）の記録も `Confirmed` にしない。`rank` も、一覧の候補は未確認のまま並べる。
- **汎用の部品が先、アダプタは癖だけ。** `Meguri.Extract.Generic` が JSON-LD → microdata（Product の範囲だけ）→ OGP の順に読み、アダプタが上書きする。
  - `Meguri.Site.<サイト>`：モール・フリマ・リユース店（楽天、Yahoo!、価格.com、ヤフオク、Yahoo!フリマ、ラクマ、ハードオフ、トレジャーファクトリー、ジモティー、ブックオフ）。
  - `Meguri.Site.DirectStores`：直販の店（ロフト、Francfranc、コーナン、ディノス、ジャパネット、イオシス、カインズ、コストコ、IKEA、ソフマップ）。共通の部品は `Meguri.Site.Store`（税込の裏づけ、「◯円以上で送料無料」、カートの読み取り、一覧のカード切り）。
  - `Meguri.Site.Captured`：ブラウザで保存したHTMLだけを読むサイト（ヨドバシ、Amazon、メルカリ、2nd STREET）。おすすめの枠を読まないよう、主の商品の id や目印だけを指す。
- **出力する文字列は整える**（`Meguri.Sanitize`）。商品名や店名はページの書き手が自由に書ける。長さを切り、制御文字を除き、URL は http(s) だけを通す。説明文は抜き出さない。ページに書かれたURLをたどるときは、公開のホストだけを取りに行く。
- **取得の決まり**（`Meguri.Fetch.Http`）：同じホストへは同時に1本・間隔2秒以上、全体の同時数は8、応答は15分キャッシュ。403・429・ボット確認は `Blocked` として、そのホストをそこでやめる。検索結果は1ページ目だけを取る。続きのページ（価格.com のシリーズ → 色ごとのショップ一覧）は、1つの検索につき4ページまで。
- フィールド名がぶつかるので `NoFieldSelectors` を使っている。値は `x.field` で読む。

## アダプタを足すとき

1. `Catalog.hs` のサイトの定義を確かめる（段・検索URL・個別ページのURLの形・注記・確かめた日・健康診断の見本の語）。個別ページのURLの形は、商品のページだけに当たるよう狭く書く（`check` が最初に当たったリンクを読む）。
2. 個別ページの要点を調べる：JSON-LD の中身、カートのボタンの目印、送料・税込・在庫の表示。直販の店なら `DirectStores` に店の決まりを足す。
3. ログインしていない状態で取ったページを、要る部分だけに切り詰めて `test/fixtures/<site>/` に置く。出品者などの個人の名前と、配達先の地名・郵便番号は入れない。
4. `test/Main.hs` の `goldenTests` に足し、`cabal test --test-options=--accept` で golden を作って、中身を目で確かめる。
5. `meguri check --only <site>` で、実際のサイトでも同じように読めるかを見る。

ブラウザでしか開けないサイトのページは、画面ありの agent-browser（ログインなし）で保存できることがある：

```bash
agent-browser --headed open "<URL>"
agent-browser eval "document.documentElement.outerHTML" > saved.txt   # JSON の文字列として出る。戻してから渡す
```

ヨドバシ・Amazon・メルカリ・2nd STREET はこれで保存できた（2026-11-21）。ビックカメラは遮断された。ブロックや確認の画面が出たら、そこでやめる。

## いまの範囲

- 検索と個別ページ：楽天（支店：ジョーシン・エディオン・2nd STREET）、Yahoo!ショッピング（2nd STREET の Yahoo!店）、ヤフオク（検索・落札相場・個別）、Yahoo!フリマ、ラクマ、ハードオフ、トレジャーファクトリー、ジモティー、ブックオフ、ロフト、Francfranc、コーナン、ディノス、ジャパネット、イオシス、カインズ。価格.com は検索とショップ一覧（個別ページとしては読まない）。
- 個別ページだけ：コストコ、IKEA、ソフマップ（新品）。ブラウザで保存したHTML：ヨドバシ、Amazon、メルカリ、2nd STREET。
- ほかのサイトは、開くべきURLの行として「どこまで探したか」に出る。`item` は汎用の抽出で読める分だけ読み、`partial` にとどめる。
- まだ無いもの：
  - HTTP の段のアダプタ：ハンズ、イシバシ楽器、BUYMA、LOHACO、au PAY など（`check` の「汎用の抽出」の行）。
  - 保存したHTMLのアダプタ：ビックカメラ、ヤマダ、ケーズ、ノジマ、ニトリ、ゲオ、KOMEHYO、マップカメラ、じゃんぱら、ZOZOTOWN、eBay など。
  - 公式API（Yahoo!ショッピング 商品検索 v3 など）と、meguri がブラウザを動かす取得（指示書の 2.5 の Phase 2。任意）。
  - 送料の計算は2サイトだけ。`verify` は、価格.com の転送URLのうちアフィリエイトの中継を挟むものは読まない。

## Phase 0 で調べて決めたこと（2026-11-20）

- **UA**：`meguri/<版> (+https://github.com/yuidvg/meguri)`。リポジトリのURLは仮（`Meguri.Fetch.Http.repoUrl`）。
- **段の確かめ直し**：HTTP可・個別ページだけHTTP可の段のサイトに、このUAで検索1回・個別1件を当てた。403 やボット確認は出なかった。いまは `meguri check` が同じことをして、結果を channels.md の「診断」に入れる。
- **TLS**：hands.net は Extended Main Secret に対応していない。tls の既定（必須）ではつながらないので、「あれば使う」設定にした（証明書の検証は外していない）。
- **公式API**（まだ実装していない。鍵の値はログとキャッシュのキーに入れない）

  | API | 決めたこと | 環境変数 |
  |---|---|---|
  | Yahoo!ショッピング 商品検索 v3 | 使う。JAN・在庫・送料区分・中古の絞り込みが取れる。送料の額は返らない | `MEGURI_YAHOO_CLIENT_ID` |
  | 楽天市場 商品検索 | 鍵があるときだけ使う。開発者ポータルが 11/9 から停止中で、新しい鍵は取れない。中古の絞り込みと送料の額は無いので、HTMLの検索を残す | `MEGURI_RAKUTEN_APP_ID`・`MEGURI_RAKUTEN_ACCESS_KEY` |
  | eBay Browse | 鍵があるときだけ使う。JPY では返らない見込み。申請なしで本番の検索が通るかは未確認 | `MEGURI_EBAY_CLIENT_ID`・`MEGURI_EBAY_CLIENT_SECRET` |
  | Amazon | 使わない（Creators API は、アソシエイトの承認と直近の成果が前提）。ブラウザの段のまま | — |

- **agent-browser**（0.38.2）：ヘッドレスでも画面ありでも、開いたページのHTMLを `eval "document.documentElement.outerHTML"` でファイルに書き出せる。JSで描画される検索（LOHACO・au PAY・コストコ・ソフマップ・IKEA）は、ヘッドレスで開いて商品リンクを拾えた。Chrome を起動しているときに `--profile Default` を使えるかは、まだ確かめていない。
