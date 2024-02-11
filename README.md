# augmented-ai-translation

This is my personal setup to translate novel chapters from Japanese to English using the LLM Claude by Anthropic, with some prompt engineering to greatly improve the output translation quality.

## Augmentations

- Chapters are translated chunk-by-chunk, solving the issue of Claude often skipping random sections of text when asked to translate very long documents.
- With each chunk, the prompt is filled with translated text from previous chapters/previously translated chunks, ensuring the AI has access to contextual information.
- With each chunk, the prompt is filled with canonized transliterations and contextual information taken from a user-maintained glossary. For example, if the current chunk contains the name "ムーノ" (Muno), the prompt will contain the information: "ムーノ = Muno (former branch manager of the Relinas Trade Company)"
    - This ensures Japanese names are translated into consistent transliterations across chapters, and that chapters are translated with access to any necessary context.
- "Prefilling" and output postprocessing is used to extract just the translation from the outputs, with no preface
- Before translating each chapter, the AI is asked if the user-maintained glossary contains enough contextual information to produce an accurate translation.
- A text file containing the untranslated AND translated version of each chunk is produced for easy human review.
- Each prompt-completion pair used is archived by the application for review.

## Example

A few chapters of the free Japanese web novel "I Shall Survive Using Potions" were translated with this app on [https://ainoveltls.wordpress.com](https://ainoveltls.wordpress.com).

## Getting started

- Clone/download this repository
- If you don't want to compile, then [download the appropriate binary for your OS](https://github.com/mtamc/augmented-ai-translation/releases) and put it at the repository's root
    - On Linux/macOS, you'll also have to run the command `chmod +x ./{BINARY FILENAME HERE}`
- In the `./playground/` directory, copy `secrets.example.json` as `secrets.json` and put your Anthropic key in it.
- Save Japanese chapter texts to `./playground/raws/` using the naming scheme `1.txt`, `2.txt`, etc.
- In the `./playground/` directory, copy `task.example.json` as `task.json`. Set your desired chapter number in the `chapter` field, write down the contextual information which you wish to be present in every prompt in the `context` array, and optionally write down some `glossary` entries.
- Ensure the `taskType` property in `task.json` is set to "AskIfContextSufficient", then run this program in a terminal (see [instructions on the release page](https://github.com/mtamc/augmented-ai-translation/releases) ). The output will be something like the following:
    ```
    Running AskIfContextSufficient (All three tasks querying for missing information)
    --------
    Running Step 1/3 - Asking if the AI needs more information, giving it the
    option to say "OK" if it has enough.)
    OK

    --------
    Running Step 2/3 - Asking if the AI needs more information, *forcing the AI
    to say at least one suggestion*. Feel free to ignore those suggestions
    if they seem overkill!

    1) Who is Coley? What is his/her role and relationship to the other characters?
    <passage>襲われたのはコーレイです、今、診療所に。</passage>
    I don't have enough context about who Coley is.

    2) What is the relationship between the Tavolas Trade Company branch
    manager/vice branch manager and Little Silver? Are they allies, competitors,
    or neutral?
    <passage>支店長と副支店長であるムーノさんのお仲間達に挨拶して、
    お茶と茶菓子を戴いていたところだ。</passage>
    It's unclear if they are friends or allies with Little Silver based on this passage.

    --------
    Running Step 3/3 - Asking for missing canonizations/transliterations in the
    glossary, *forcing the AI to say at least one suggestion*. Again, feel free
    to ignore those suggestions if they seem overkill!
     コーレイ (Korei)
    - リトルシルバー (Little Silver)
    ```
- Based on the output of AskIfContextSufficient, update the glossary in `task.json`, or don't. Some human judgment is required here.
- Change the `taskType` property in `task.json` to "Translate". Run this program in a terminal. A file will be created at `./playground/translations/{CHAPTER_NUMBER}.txt`, containing the untranslated and translated version of each chunk, for easy human review.
- All prompt-completion pairs used in this process are saved to `./playground/prompts/`
