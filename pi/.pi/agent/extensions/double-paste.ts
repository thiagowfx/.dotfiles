import { CustomEditor, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

const PASTE_START = "\x1b[200~";
const PASTE_END = "\x1b[201~";
const PASTE_MARKER = /\[paste #(\d+)(?: (?:\+\d+ lines|\d+ chars))?\]/g;

function normalizePaste(text: string): string {
  const decoded = text.replace(/\x1b\[(\d+);5u/g, (match, code) => {
    const codePoint = Number(code);
    if (codePoint >= 97 && codePoint <= 122) return String.fromCharCode(codePoint - 96);
    if (codePoint >= 65 && codePoint <= 90) return String.fromCharCode(codePoint - 64);
    return match;
  });

  return decoded
    .replace(/\r\n/g, "\n")
    .replace(/\r/g, "\n")
    .replace(/\t/g, "    ")
    .split("")
    .filter((char) => char === "\n" || char.charCodeAt(0) >= 32)
    .join("");
}

export class DoublePasteEditor extends CustomEditor {
  private doublePasteBuffer = "";
  private isInDoublePaste = false;
  private expansionHintVisible = false;

  constructor(
    tui: ConstructorParameters<typeof CustomEditor>[0],
    theme: ConstructorParameters<typeof CustomEditor>[1],
    keybindings: ConstructorParameters<typeof CustomEditor>[2],
    private readonly onCollapse: () => void,
    private readonly onClearHint: () => void,
  ) {
    super(tui, theme, keybindings);
  }

  private getNativePastes(): Map<number, string> {
    return (this as unknown as { pastes: Map<number, string> }).pastes;
  }

  private showExpansionHint(): void {
    this.expansionHintVisible = true;
    this.onCollapse();
  }

  private clearExpansionHint(): void {
    if (!this.expansionHintVisible) return;
    this.expansionHintVisible = false;
    this.onClearHint();
  }

  private expandMatchingPaste(pastedText: string): boolean {
    if (!this.expansionHintVisible) return false;

    const matches = [...this.getText().matchAll(PASTE_MARKER)];
    const latest = matches.at(-1);
    if (!latest) return false;

    const storedText = this.getNativePastes().get(Number(latest[1]));
    if (storedText === undefined) return false;

    const normalized = normalizePaste(pastedText);
    if (storedText !== normalized && storedText !== ` ${normalized}`) return false;

    this.setText(this.getExpandedText());
    this.clearExpansionHint();
    return true;
  }

  private finishPaste(text: string): void {
    if (this.expandMatchingPaste(text)) return;

    const markersBefore = [...this.getText().matchAll(PASTE_MARKER)].length;
    super.handleInput(`${PASTE_START}${text}${PASTE_END}`);
    const markersAfter = [...this.getText().matchAll(PASTE_MARKER)].length;

    if (markersAfter > markersBefore) {
      this.showExpansionHint();
    } else {
      this.clearExpansionHint();
    }
  }

  override handleInput(data: string): void {
    const start = data.indexOf(PASTE_START);
    if (!this.isInDoublePaste && start !== -1) {
      if (start > 0) super.handleInput(data.slice(0, start));
      this.isInDoublePaste = true;
      this.doublePasteBuffer = data.slice(start + PASTE_START.length);
    } else if (this.isInDoublePaste) {
      this.doublePasteBuffer += data;
    } else {
      this.clearExpansionHint();
      super.handleInput(data);
      return;
    }

    const end = this.doublePasteBuffer.indexOf(PASTE_END);
    if (end === -1) return;

    const pastedText = this.doublePasteBuffer.slice(0, end);
    const remaining = this.doublePasteBuffer.slice(end + PASTE_END.length);
    this.doublePasteBuffer = "";
    this.isInDoublePaste = false;
    this.finishPaste(pastedText);
    if (remaining) this.handleInput(remaining);
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    const clearHint = () => ctx.ui.setStatus("double-paste", undefined);
    ctx.ui.setEditorComponent((tui, theme, keybindings) =>
      new DoublePasteEditor(
        tui,
        theme,
        keybindings,
        () => ctx.ui.setStatus("double-paste", ctx.ui.theme.fg("dim", "paste again to expand")),
        clearHint,
      ),
    );
  });

  pi.on("before_agent_start", (_event, ctx) => {
    if (ctx.mode === "tui") ctx.ui.setStatus("double-paste", undefined);
  });
}
