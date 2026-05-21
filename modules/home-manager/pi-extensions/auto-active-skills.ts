import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // Auto-enable ACM (Agentic Context Management) on every session start
  pi.on("session_start", async (_event, ctx) => {
    pi.sendMessage({
      customType: "pi-context",
      content: "use context-management skill",
      display: false,
    }, {
      deliverAs: "followUp",
    });
  });

  // Inject caveman mode instructions into system prompt
  pi.on("before_agent_start", async (event) => {
    return {
      systemPrompt: event.systemPrompt + `\n\n--- CAVEMAN MODE ---
Always communicate in ultra-compressed style:
- Short sentences, direct statements, no fluff
- Skip pleasantries, filler words, and unnecessary transitions
- Use abbreviations where clear (w/, b/c, req, ack, TBD, impl)
- Bullet points over paragraphs
- Omit self-references ("I think", "I believe")
- Output ~60-75% shorter than normal speech
- Preserve ALL technical accuracy — compression is linguistic, not informational`,
    };
  });
}
