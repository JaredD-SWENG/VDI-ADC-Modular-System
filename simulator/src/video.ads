
with Gtkada.Builder;          use Gtkada.Builder;

package Video is

   procedure Init (B : Gtkada_Builder);
   function Frame_Callback return Boolean;

end Video;