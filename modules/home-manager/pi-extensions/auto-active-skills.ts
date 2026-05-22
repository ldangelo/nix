import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  // Auto-enable ACM (Agentic Context Management) on session startup.
  // /acm is the pi-context command that both binds command context (needed by
  // context_checkout) and queues the context-management skill load. A custom
  // hidden message only told the model to use the skill; it did not enable ACM.
  pi.on("session_start", async (event) => {
    if (event.reason === "reload") return;

    pi.sendUserMessage("/acm", {
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
