export default function (md: any) {

  // The tokenize function to add.
  interface Token {
    content: string;
    markup: string;
    map: [number, number];
  }

  interface State {
    pos: number;
    src: string;
    inline: {
      ruler: {
        before: (beforeName: string, ruleName: string, fn: (state: State, silent: boolean) => boolean) => void;
      };
    };
    push: (type: string, tag: string, nesting: number) => Token;
  }

  function tokenize(state: State, silent: boolean): boolean {
    const start = state.pos;
    const marker = state.src.charCodeAt(start);

    if (silent) {
      return false;
    }

    if (marker !== 0x5B /* [ */ || state.src.charCodeAt(start + 1) !== 0x5B /* [ */) {
      return false;
    }

    const match = state.src.slice(start).match(/\[\[.*?\]\]/);
    if (!match) {
      return false;
    }

    const matchStr = match[0];
    const innerPerspectivesExpr = matchStr.slice(2, -2);

    if (!isPerspectivesSyntax(innerPerspectivesExpr)) {
      return false;
    }

    state.pos += matchStr.length;

    const token = state.push('perspectives', '', 0);
    token.content = innerPerspectivesExpr;
    token.markup = matchStr;
    token.map = [start, state.pos];

    return true;
  }

  // Insert the tokenizer into markdown-it.
  md.inline.ruler.before('emphasis', 'perspectives', tokenize);

  // Add a renderer function.
  md.renderer.rules.perspectives = function(tokens: Token[], idx: number): string | undefined {
    // Render appropriate content for each of the extensions.
    return render(tokens[idx].content);
  };
};


function render (s : string) : string | undefined
{
  let matchResult;
  matchResult = s.match(linkRegEx)
  if (matchResult) {
    const roleIdentifier = matchResult[1];
    const readableText = matchResult[2];
    return `<a href="#" onclick="$perspectives_entry_point_for_markdown$.opencontext(event, '${roleIdentifier}')">${readableText}</a>`
  }

  matchResult = s.match(actionRegEx);
  if (matchResult) {
    const action = matchResult[1];
    const readableText = matchResult[2];
    return `<button type="button" class="btn btn-primary btn-sm" onclick="$perspectives_entry_point_for_markdown$.runaction(event, '${action}', '${readableText}', '__contextid__', '__myroletype__')">${readableText}</Button>`
  }

}

// [[link: <roleIdentifier>|Leesbare link tekst]]
const linkRegEx = /link:\s*([^|]+)\s*\|\s*(.+)/;
// [[action: <action name>| leesbare actie tekst ]]
const actionRegEx = /action:\s*(.+)\s*\|\s*(.+)/;
// [[expression: <expression> ]]

// Check whether the string represents valid Perspectives extension syntax.
function isPerspectivesSyntax(s : string) : boolean
{
  let correct = false;
  if (s.match(linkRegEx)) {
    correct = true;
  }
  if (s.match(actionRegEx)) {
    correct = true;
  }
  return correct;
}